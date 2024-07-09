open Stdune
open Jsonrpc
open Jsonrpc_fiber
open Fiber.O
open Fiber.Stream

module Stream_chan = struct
  type t = Jsonrpc.Packet.t In.t * Jsonrpc.Packet.t Out.t

  let close (_, o) what =
    match what with
    | `Read -> Fiber.return ()
    | `Write -> Out.write o None
  ;;

  let send (_, o) p = Fiber.sequential_iter p ~f:(fun x -> Out.write o (Some x))
  let recv (i, _) = In.read i
end

module Jrpc = Jsonrpc_fiber.Make (Stream_chan)
module Context = Jrpc.Context

let print_json json = print_endline (Yojson.Safe.pretty_to_string ~std:false json)

let no_output () =
  let received_none = ref false in
  Out.create (function
    | None ->
      if !received_none
      then failwith "received None more than once"
      else received_none := true;
      Fiber.return ()
    | Some _ -> failwith "unexpected element")
;;

let%expect_test "start and stop server" =
  let run () =
    let in_ = In.of_list [] in
    let jrpc = Jrpc.create ~name:"test" (in_, no_output ()) () in
    let run = Jrpc.run jrpc in
    Fiber.fork_and_join_unit (fun () -> run) (fun () -> Jrpc.stop jrpc)
  in
  let () = Fiber_test.test Dyn.opaque run in
  [%expect {|
    <opaque> |}]
;;

let%expect_test "server accepts notifications" =
  let notif =
    { Jsonrpc.Notification.method_ = "method"; params = Some (`List [ `String "bar" ]) }
  in
  let run () =
    let in_ = In.of_list [ Jsonrpc.Packet.Notification notif ] in
    let on_notification c =
      let n = Context.message c in
      let state = Context.state c in
      assert (notif = n);
      print_endline "received notification";
      Fiber.return (Notify.Stop, state)
    in
    let jrpc = Jrpc.create ~name:"test" ~on_notification (in_, no_output ()) () in
    Jrpc.run jrpc
  in
  Fiber_test.test Dyn.opaque run;
  [%expect {|
    received notification
    <opaque> |}]
;;

let of_ref ref =
  Fiber.Stream.Out.create (function
    | None -> Fiber.return ()
    | Some x ->
      ref := x :: !ref;
      Fiber.return ())
;;

let%expect_test "serving requests" =
  let id = `Int 1 in
  let request =
    { Jsonrpc.Request.id; method_ = "bla"; params = Some (`List [ `Int 100 ]) }
  in
  let response_data = `String "response" in
  let run () =
    let responses = ref [] in
    let in_ = In.of_list [ Jsonrpc.Packet.Request request ] in
    let on_request c =
      let r = Context.message c in
      let state = Context.state c in
      assert (r = request);
      let response = Jsonrpc.Response.ok r.id response_data in
      Fiber.return (Reply.now response, state)
    in
    let out = of_ref responses in
    let jrpc = Jrpc.create ~name:"test" ~on_request (in_, out) () in
    let+ () = Jrpc.run jrpc in
    List.iter !responses ~f:(fun resp ->
      let json = Jsonrpc.Packet.yojson_of_t resp in
      print_endline (Yojson.Safe.pretty_to_string ~std:false json))
  in
  Fiber_test.test Dyn.opaque run;
  [%expect {|
    { "id": 1, "jsonrpc": "2.0", "result": "response" }
    <opaque> |}]
;;

(* The current client/server implement has no concurrent handling of requests.
   We can show this when we try to send a request when handling a response. *)
let%expect_test "concurrent requests" =
  let print packet =
    print_endline
      (Yojson.Safe.pretty_to_string ~std:false (Jsonrpc.Packet.yojson_of_t packet))
  in
  let waiter chan =
    let on_request c =
      let self = Context.session c in
      let request = Context.message c in
      print_endline "waiter: received request";
      print (Request request);
      let response =
        Reply.later (fun send ->
          print_endline "waiter: sending response";
          let* () = send (Jsonrpc.Response.ok request.id `Null) in
          print_endline "waiter: making request";
          let* response =
            let request = Jsonrpc.Request.create ~id:(`Int 100) ~method_:"shutdown" () in
            Jrpc.request self request
          in
          print_endline "waiter: received response:";
          print (Response response);
          let* () = send (Jsonrpc.Response.ok request.id `Null) in
          print_endline "waiter: stopping";
          let+ () = Jrpc.stop self in
          print_endline "waiter: stopped")
      in
      Fiber.return (response, ())
    in
    Jrpc.create ~name:"waiter" ~on_request chan ()
  in
  let waitee chan =
    let on_request c =
      print_endline "waitee: received request";
      let request = Context.message c in
      print (Request request);
      let response =
        Reply.later (fun send ->
          let* () = send (Jsonrpc.Response.ok request.id (`Int 42)) in
          if request.method_ = "shutdown"
          then (
            let self = Context.session c in
            print_endline "waitee: stopping";
            let+ () = Jrpc.stop self in
            print_endline "waitee: stopped")
          else Fiber.return ())
      in
      let state = Context.state c in
      Fiber.return (response, state)
    in
    Jrpc.create ~on_request ~name:"waitee" chan ()
  in
  let waitee_in, waiter_out = pipe () in
  let waiter_in, waitee_out = pipe () in
  let waitee = waitee (waitee_in, waitee_out) in
  let waiter = waiter (waiter_in, waiter_out) in
  let run () =
    let initial_request () =
      let request = Jsonrpc.Request.create ~id:(`String "initial") ~method_:"init" () in
      print_endline "initial: waitee requests from waiter";
      let+ resp = Jrpc.request waitee request in
      print_endline "initial request response:";
      print (Response resp)
    in
    Fiber.all_concurrently_unit [ Jrpc.run waitee; initial_request (); Jrpc.run waiter ]
  in
  Fiber_test.test Dyn.opaque run;
  [%expect
    {|
    initial: waitee requests from waiter
    waiter: received request
    { "id": "initial", "method": "init", "jsonrpc": "2.0" }
    waiter: sending response
    waiter: making request
    waitee: received request
    { "id": 100, "method": "shutdown", "jsonrpc": "2.0" }
    waitee: stopping
    waitee: stopped
    initial request response:
    { "id": "initial", "jsonrpc": "2.0", "result": null }
    waiter: received response:
    { "id": 100, "jsonrpc": "2.0", "result": 42 }
    [FAIL] unexpected Never raised |}]
;;

let%expect_test "test from jsonrpc_test.ml" =
  Printexc.record_backtrace false;
  let response =
    let i = ref 0 in
    fun () ->
      incr i;
      `Int !i
  in
  let on_request ctx =
    let req : Jsonrpc.Request.t = Context.message ctx in
    let state = Context.state ctx in
    Fiber.return (Reply.now (Jsonrpc.Response.ok req.id (response ())), state)
  in
  let on_notification ctx =
    let n : Jsonrpc.Notification.t = Context.message ctx in
    if n.method_ = "raise" then failwith "special failure";
    let json = Notification.yojson_of_t n in
    print_endline ">> received notification";
    print_json json;
    Fiber.return (Jsonrpc_fiber.Notify.Continue, ())
  in
  let responses = ref [] in
  let initial_requests =
    let request ?params id method_ : Jsonrpc.Packet.t =
      Request (Jsonrpc.Request.create ?params ~id ~method_ ())
    in
    let notification ?params method_ : Jsonrpc.Packet.t =
      Notification (Jsonrpc.Notification.create ?params ~method_ ())
    in
    [ request (`Int 10) "foo"
    ; request (`String "testing") "bar"
    ; notification "notif1"
    ; notification "notif2"
    ; notification "raise"
    ]
  in
  let reqs_in, reqs_out = pipe () in
  let chan =
    let out = of_ref responses in
    reqs_in, out
  in
  let session = Jrpc.create ~on_notification ~on_request ~name:"test" chan () in
  let write_reqs () =
    let* () =
      Fiber.sequential_iter initial_requests ~f:(fun req -> Out.write reqs_out (Some req))
    in
    Out.write reqs_out None
  in
  Fiber_test.test Dyn.opaque (fun () ->
    Fiber.fork_and_join_unit write_reqs (fun () -> Jrpc.run session));
  List.rev !responses
  |> List.iter ~f:(fun packet ->
    let json = Jsonrpc.Packet.yojson_of_t packet in
    print_json json);
  [%expect
    {|
    >> received notification
    { "method": "notif1", "jsonrpc": "2.0" }
    >> received notification
    { "method": "notif2", "jsonrpc": "2.0" }
    Uncaught error when handling notification:
    { "method": "raise", "jsonrpc": "2.0" }
    Error:
    [ { exn = "Failure(\"special failure\")"; backtrace = "" } ]
    <opaque>
    { "id": 10, "jsonrpc": "2.0", "result": 1 }
    { "id": "testing", "jsonrpc": "2.0", "result": 2 } |}]
;;

let%expect_test "cancellation" =
  let () = Printexc.record_backtrace true in
  let print packet =
    print_endline
      (Yojson.Safe.pretty_to_string ~std:false (Jsonrpc.Packet.yojson_of_t packet))
  in
  let server_req_ack = Fiber.Ivar.create () in
  let client_req_ack = Fiber.Ivar.create () in
  let server chan =
    let on_request c =
      let request = Context.message c in
      let state = Context.state c in
      print_endline "server: received request";
      print (Request request);
      let* () = Fiber.Ivar.fill server_req_ack () in
      let response =
        Reply.later (fun send ->
          print_endline "server: waiting for client ack before sending response";
          let* () = Fiber.Ivar.read client_req_ack in
          print_endline "server: got client ack, sending response";
          send (Jsonrpc.Response.ok request.id (`String "Ok")))
      in
      Fiber.return (response, state)
    in
    Jrpc.create ~name:"server" ~on_request chan ()
  in
  let client chan = Jrpc.create ~name:"client" chan () in
  let run () =
    let client_in, client_out = pipe () in
    let server_in, server_out = pipe () in
    let client = client (client_in, server_out) in
    let server = server (server_in, client_out) in
    let request = Jsonrpc.Request.create ~id:(`String "initial") ~method_:"init" () in
    let cancel, req = Jrpc.request_with_cancel client request in
    let fire_cancellation =
      let* () = Fiber.return () in
      print_endline "client: waiting for server ack before cancelling request";
      let* () = Fiber.Ivar.read server_req_ack in
      print_endline "client: got server ack, cancelling request";
      let* () = Jrpc.fire cancel in
      Fiber.Ivar.fill client_req_ack ()
    in
    let initial_request =
      let* () = Fiber.return () in
      print_endline "client: sending request";
      let+ resp = req in
      match resp with
      | `Cancelled -> print_endline "request has been cancelled"
      | `Ok resp ->
        print_endline "request response:";
        print (Response resp)
    in
    Fiber.all_concurrently
      [ fire_cancellation
      ; Jrpc.run client
      ; initial_request
        >>> Fiber.fork_and_join_unit
              (fun () -> Out.write server_out None >>> Jrpc.stop client)
              (fun () -> Jrpc.stop server)
      ; Jrpc.run server
      ; Jrpc.stopped client
      ; Jrpc.stopped server
      ]
  in
  Fiber_test.test Dyn.opaque run;
  [%expect
    {|
    client: waiting for server ack before cancelling request
    client: sending request
    server: received request
    { "id": "initial", "method": "init", "jsonrpc": "2.0" }
    server: waiting for client ack before sending response
    client: got server ack, cancelling request
    request has been cancelled
    server: got client ack, sending response
    <opaque> |}]
;;
