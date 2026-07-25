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

module Response_before_send_chan = struct
  type t =
    { input : Jsonrpc.Packet.t In.t
    ; output : Jsonrpc.Packet.t Out.t
    ; response_read : unit Fiber.Ivar.t option
    ; send_returned : unit Fiber.Ivar.t option
    ; mutable closed : bool
    }

  let close t _ =
    if t.closed
    then Fiber.return ()
    else (
      t.closed <- true;
      Out.write t.output None)
  ;;

  let send t packets =
    let* () =
      Fiber.sequential_iter packets ~f:(fun packet -> Out.write t.output (Some packet))
    in
    match t.response_read, packets with
    | Some response_read, (Request _ :: _ | Batch_call _ :: _) ->
      let* () = Fiber.Ivar.read response_read in
      (match t.send_returned with
       | Some send_returned -> Fiber.Ivar.fill send_returned ()
       | None -> Fiber.return ())
    | Some _, (Notification _ :: _ | Response _ :: _ | Batch_response _ :: _ | [])
    | None, _ -> Fiber.return ()
  ;;

  let recv t =
    let* packet = In.read t.input in
    match packet, t.response_read with
    | Some (Response _ | Batch_response _), Some response_read ->
      let+ () = Fiber.Ivar.fill response_read () in
      packet
    | Some (Notification _ | Request _ | Batch_call _), Some _ | Some _, None | None, _ ->
      Fiber.return packet
  ;;
end

module Response_before_send_jrpc = Jsonrpc_fiber.Make (Response_before_send_chan)

module Recording_chan = struct
  type t =
    { input : Jsonrpc.Packet.t In.t
    ; sent : Jsonrpc.Packet.t list list ref
    }

  let close _ _ = Fiber.return ()

  let send t packets =
    t.sent := packets :: !(t.sent);
    Fiber.return ()
  ;;

  let recv t = In.read t.input
end

module Recording_jrpc = Jsonrpc_fiber.Make (Recording_chan)

module Failing_send_chan = struct
  type t =
    { input : Jsonrpc.Packet.t In.t
    ; attempts : Jsonrpc.Packet.t list list ref
    }

  let close _ _ = Fiber.return ()

  let send t packets =
    Fiber.of_thunk (fun () ->
      t.attempts := packets :: !(t.attempts);
      failwith "send failed")
  ;;

  let recv t = In.read t.input
end

module Failing_send_jrpc = Jsonrpc_fiber.Make (Failing_send_chan)

let print_json json = print_endline (Yojson.Safe.pretty_to_string ~std:false json)

let print_packets label packets =
  Printf.printf "%s:\n" label;
  print_json (`List (List.map packets ~f:Jsonrpc.Packet.yojson_of_t))
;;

let print_packet_groups label groups =
  Printf.printf "%s:\n" label;
  print_json
    (`List
        (List.map groups ~f:(fun packets ->
           `List (List.map packets ~f:Jsonrpc.Packet.yojson_of_t))))
;;

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
  [%expect
    {|
    <opaque> |}]
;;

let%expect_test "cleanup precedes explicit output close" =
  let output =
    Out.create (function
      | None ->
        print_endline "output closed";
        Fiber.return ()
      | Some _ ->
        print_endline "notification sent";
        Fiber.return ())
  in
  let jrpc = Jrpc.create ~name:"test" (In.of_list [], output) () in
  let cleanup () =
    print_endline "stopping";
    let notification = Jsonrpc.Notification.create ~method_:"cleanup" () in
    Jrpc.notification jrpc notification
  in
  Fiber_test.test Dyn.opaque (fun () ->
    Fiber.finalize
      (fun () ->
         let* () = Jrpc.run_until_stopped jrpc in
         cleanup ())
      ~finally:(fun () -> Jrpc.close jrpc));
  [%expect
    {|
    stopping
    notification sent
    output closed
    <opaque> |}]
;;

let%expect_test "close shares failures between callers" =
  let close_attempts = ref 0 in
  let output =
    Out.create (function
      | None ->
        incr close_attempts;
        failwith "close failed"
      | Some _ -> Fiber.return ())
  in
  let jrpc = Jrpc.create ~name:"test" (In.of_list [], output) () in
  let close () =
    let+ result = Fiber.collect_errors (fun () -> Jrpc.close jrpc) in
    print_endline
      (match result with
       | Ok () -> "close succeeded"
       | Error _ -> "close failed")
  in
  Fiber_test.test Dyn.opaque (fun () ->
    let* () = Fiber.fork_and_join_unit close close in
    let+ () = close () in
    Printf.printf "close attempts: %d\n" !close_attempts);
  [%expect
    {|
    close failed
    close failed
    close failed
    close attempts: 1
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
  [%expect
    {|
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
  [%expect
    {|
    { "id": 1, "jsonrpc": "2.0", "result": "response" }
    <opaque> |}]
;;

let%expect_test "delayed replies may issue concurrent requests" =
  let finished = Fiber.Ivar.create () in
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
          print_endline "waiter: stopping";
          let* () = Jrpc.stop self in
          let+ () = Fiber.Ivar.fill finished () in
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
    let close_streams =
      let* () = Fiber.Ivar.read finished in
      Fiber.fork_and_join_unit
        (fun () -> Out.write waiter_out None)
        (fun () -> Out.write waitee_out None)
    in
    Fiber.all_concurrently_unit
      [ Jrpc.run waitee; initial_request (); Jrpc.run waiter; close_streams ]
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
    waiter: stopping
    waiter: stopped
    <opaque> |}]
;;

let%expect_test "request exceptions become JSON-RPC errors" =
  Printexc.record_backtrace false;
  let requests =
    [ Jsonrpc.Request.create ~id:(`Int 1) ~method_:"invalid-params" ()
    ; Jsonrpc.Request.create ~id:(`Int 2) ~method_:"crash" ()
    ]
  in
  let responses = ref [] in
  let on_request context =
    let request : Jsonrpc.Request.t = Context.message context in
    match request.method_ with
    | "invalid-params" ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InvalidParams ~message:"invalid parameters" ())
    | "crash" -> failwith "handler crashed"
    | _ -> assert false
  in
  let input = List.map requests ~f:(fun request -> Jsonrpc.Packet.Request request) in
  let session =
    Jrpc.create ~name:"server" ~on_request (In.of_list input, of_ref responses) ()
  in
  Fiber_test.test Dyn.opaque (fun () -> Jrpc.run session);
  List.rev !responses
  |> List.iter ~f:(function
    | Jsonrpc.Packet.Response { id; result = Error error } ->
      Printf.printf
        "%s: %s: %s\n"
        (Yojson.Safe.to_string (Jsonrpc.Id.yojson_of_t id))
        (Jsonrpc.Response.Error.Code.to_string error.code)
        error.message
    | Notification _ | Request _ | Response _ | Batch_call _ | Batch_response _ ->
      print_endline "unexpected packet");
  [%expect
    {|
    <opaque>
    1: InvalidParams: invalid parameters
    2: InternalError: uncaught exception |}]
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

let%expect_test "a response received before send returns races with cancellation" =
  let response_read = Fiber.Ivar.create () in
  let send_returned = Fiber.Ivar.create () in
  let client_input, server_output = pipe () in
  let server_input, client_output = pipe () in
  let client_channel : Response_before_send_chan.t =
    { input = client_input
    ; output = client_output
    ; response_read = Some response_read
    ; send_returned = Some send_returned
    ; closed = false
    }
  in
  let server_channel : Response_before_send_chan.t =
    { input = server_input
    ; output = server_output
    ; response_read = None
    ; send_returned = None
    ; closed = false
    }
  in
  let client = Response_before_send_jrpc.create ~name:"client" client_channel () in
  let server =
    let on_request context =
      let request : Jsonrpc.Request.t =
        Response_before_send_jrpc.Context.message context
      in
      let state = Response_before_send_jrpc.Context.state context in
      Fiber.return
        (Reply.now (Jsonrpc.Response.ok request.id (`String "immediate")), state)
    in
    Response_before_send_jrpc.create ~name:"server" ~on_request server_channel ()
  in
  let run () =
    let request = Jsonrpc.Request.create ~id:(`Int 1) ~method_:"immediate" () in
    let request () =
      let cancel, response =
        Response_before_send_jrpc.request_with_cancel client request
      in
      let cancel_after_send () =
        let* () = Fiber.Ivar.read send_returned in
        Response_before_send_jrpc.fire cancel
      in
      let* (), response = Fiber.fork_and_join cancel_after_send (fun () -> response) in
      (match response with
       | `Cancelled -> print_endline "response: dropped"
       | `Ok { result = Ok (`String value); _ } -> Printf.printf "response: %s\n" value
       | `Ok _ -> print_endline "unexpected response");
      Fiber.fork_and_join_unit
        (fun () -> Response_before_send_jrpc.stop client)
        (fun () -> Response_before_send_jrpc.stop server)
    in
    Fiber.all_concurrently_unit
      [ Response_before_send_jrpc.run client
      ; Response_before_send_jrpc.run server
      ; request ()
      ]
  in
  Fiber_test.test Dyn.opaque run;
  [%expect.unreachable]
[@@expect.uncaught_exn {| (Failure Fiber.Ivar.fill) |}]
;;

let%expect_test "request IDs may be retried after send errors" =
  let incoming, incoming_writer = pipe () in
  let attempts = ref [] in
  let channel : Failing_send_chan.t = { input = incoming; attempts } in
  let session = Failing_send_jrpc.create ~name:"client" channel () in
  let request = Jsonrpc.Request.create ~id:(`Int 1) ~method_:"retry" () in
  let operations () =
    let send () =
      Fiber.collect_errors (fun () -> Failing_send_jrpc.request session request)
    in
    let* first = send () in
    let* second = send () in
    let classify = function
      | Error [ _ ] -> "error"
      | Ok _ | Error _ -> "unexpected"
    in
    Printf.printf "first: %s\n" (classify first);
    Printf.printf "retry: %s\n" (classify second);
    print_packet_groups "wire attempts" (List.rev !attempts);
    Out.write incoming_writer None
  in
  Fiber_test.test Dyn.opaque (fun () ->
    Fiber.fork_and_join_unit (fun () -> Failing_send_jrpc.run session) operations);
  [%expect
    {|
    first: error
    retry: error
    wire attempts:
    [
      [ { "id": 1, "method": "retry", "jsonrpc": "2.0" } ],
      [ { "id": 1, "method": "retry", "jsonrpc": "2.0" } ]
    ]
    <opaque> |}]
;;

let%expect_test "duplicate request IDs are sent before rejection" =
  let first_sent = Fiber.Ivar.create () in
  let sent = ref [] in
  let incoming, incoming_writer = pipe () in
  let output : Jsonrpc.Packet.t Out.t =
    Out.create (function
      | Some packet ->
        sent := packet :: !sent;
        (match packet with
         | Jsonrpc.Packet.Request _ ->
           let* first = Fiber.Ivar.peek first_sent in
           (match first with
            | Some () -> Fiber.return ()
            | None -> Fiber.Ivar.fill first_sent ())
         | Notification _ | Response _ | Batch_call _ | Batch_response _ ->
           Fiber.return ())
      | None -> Fiber.return ())
  in
  let session = Jrpc.create ~name:"client" (incoming, output) () in
  let request = Jsonrpc.Request.create ~id:(`Int 1) ~method_:"duplicate" () in
  let classify = function
    | Ok _ -> "answered"
    | Error [ _ ] -> "rejected"
    | Error _ -> "unexpected errors"
  in
  let operations () =
    let first () = Fiber.collect_errors (fun () -> Jrpc.request session request) in
    let duplicate () =
      let* () = Fiber.Ivar.read first_sent in
      let* result = Fiber.collect_errors (fun () -> Jrpc.request session request) in
      let packets = List.rev !sent in
      let* () = Jrpc.stop session in
      let+ () = Out.write incoming_writer None in
      result, packets
    in
    let+ first, (duplicate, packets) = Fiber.fork_and_join first duplicate in
    Printf.printf "first: %s\n" (classify first);
    Printf.printf "duplicate: %s\n" (classify duplicate);
    print_packets "wire packets" packets
  in
  Fiber_test.test Dyn.opaque (fun () ->
    Fiber.fork_and_join_unit (fun () -> Jrpc.run session) operations);
  [%expect
    {|
    first: rejected
    duplicate: rejected
    wire packets:
    [
      { "id": 1, "method": "duplicate", "jsonrpc": "2.0" },
      { "id": 1, "method": "duplicate", "jsonrpc": "2.0" }
    ]
    <opaque> |}]
;;

let%expect_test "submitting a batch sends one ordered packet group" =
  let incoming, incoming_writer = pipe () in
  let sent = ref [] in
  let channel : Recording_chan.t = { input = incoming; sent } in
  let session = Recording_jrpc.create ~name:"client" channel () in
  let batch = Recording_jrpc.Batch.create () in
  let notification = Jsonrpc.Notification.create ~method_:"first" () in
  Recording_jrpc.Batch.notification batch notification;
  let request id method_ = Jsonrpc.Request.create ~id:(`Int id) ~method_ () in
  let first = Recording_jrpc.Batch.request batch (request 1 "second") in
  let second = Recording_jrpc.Batch.request batch (request 2 "third") in
  let run_batch () =
    let* () = Recording_jrpc.submit session batch in
    let methods =
      match !sent with
      | [ packets ] ->
        List.map packets ~f:(function
          | Jsonrpc.Packet.Notification notification -> notification.method_
          | Request request -> request.method_
          | Response _ | Batch_call _ | Batch_response _ -> "unexpected")
      | _ -> [ "unexpected send count" ]
    in
    Printf.printf "sent: %s\n" (String.concat ~sep:", " methods);
    let* () =
      Out.write
        incoming_writer
        (Some (Jsonrpc.Packet.Response (Jsonrpc.Response.ok (`Int 1) (`String "one"))))
    in
    let* () =
      Out.write
        incoming_writer
        (Some (Jsonrpc.Packet.Response (Jsonrpc.Response.ok (`Int 2) (`String "two"))))
    in
    let* first, second =
      Fiber.fork_and_join
        (fun () -> Recording_jrpc.Batch.await first)
        (fun () -> Recording_jrpc.Batch.await second)
    in
    let result response =
      match response.Jsonrpc.Response.result with
      | Ok (`String value) -> value
      | Ok _ | Error _ -> "unexpected"
    in
    Printf.printf "responses: %s, %s\n" (result first) (result second);
    Out.write incoming_writer None
  in
  Fiber_test.test Dyn.opaque (fun () ->
    Fiber.fork_and_join_unit (fun () -> Recording_jrpc.run session) run_batch);
  [%expect
    {|
    sent: first, second, third
    responses: one, two
    <opaque> |}]
;;

let%expect_test "stopping a session wakes pending requests" =
  let request_sent = Fiber.Ivar.create () in
  let incoming, incoming_writer = pipe () in
  let output : Jsonrpc.Packet.t Out.t =
    Out.create (function
      | Some (Jsonrpc.Packet.Request _) -> Fiber.Ivar.fill request_sent ()
      | Some (Jsonrpc.Packet.Notification _ | Response _ | Batch_call _ | Batch_response _)
      | None -> Fiber.return ())
  in
  let session = Jrpc.create ~name:"client" (incoming, output) () in
  let request = Jsonrpc.Request.create ~id:(`Int 1) ~method_:"pending" () in
  let pending_request () =
    let+ result = Fiber.collect_errors (fun () -> Jrpc.request session request) in
    match result with
    | Error [ { Exn_with_backtrace.exn = Jsonrpc_fiber.Stopped stopped; _ } ]
      when stopped = request -> print_endline "request stopped"
    | Ok _ | Error _ -> print_endline "unexpected result"
  in
  let stop () =
    let* () = Fiber.Ivar.read request_sent in
    Jrpc.stop session
  in
  let run () =
    Fiber.fork_and_join_unit
      (fun () -> Jrpc.run session)
      (fun () ->
         let* (), () = Fiber.fork_and_join pending_request stop in
         Out.write incoming_writer None)
  in
  Fiber_test.test Dyn.opaque run;
  [%expect
    {|
    request stopped
    <opaque> |}]
;;

let%expect_test "cancelling before a request starts still sends it" =
  let incoming, incoming_writer = pipe () in
  let sent = ref [] in
  let session = Jrpc.create ~name:"client" (incoming, of_ref sent) () in
  let request = Jsonrpc.Request.create ~id:(`Int 1) ~method_:"cancel" () in
  let cancel, response = Jrpc.request_with_cancel session request in
  let run () =
    Fiber.fork_and_join_unit
      (fun () -> Jrpc.run session)
      (fun () ->
         let* () = Jrpc.fire cancel in
         let* response = response in
         (match response with
          | `Cancelled -> print_endline "cancelled"
          | `Ok _ -> print_endline "unexpected response");
         print_packets "wire packets" (List.rev !sent);
         Out.write incoming_writer None)
  in
  Fiber_test.test Dyn.opaque run;
  [%expect
    {|
    cancelled
    wire packets:
    [ { "id": 1, "method": "cancel", "jsonrpc": "2.0" } ]
    <opaque> |}]
;;

let%expect_test "cancelled request IDs remain registered" =
  let request_sent = Fiber.Mvar.create () in
  let incoming, incoming_writer = pipe () in
  let output : Jsonrpc.Packet.t Out.t =
    Out.create (function
      | Some (Jsonrpc.Packet.Request _) -> Fiber.Mvar.write request_sent ()
      | Some (Jsonrpc.Packet.Notification _ | Response _ | Batch_call _ | Batch_response _)
        -> Fiber.return ()
      | None -> Fiber.return ())
  in
  let session = Jrpc.create ~name:"client" (incoming, output) () in
  let request = Jsonrpc.Request.create ~id:(`Int 1) ~method_:"cancel" () in
  let run_request () =
    let cancel, response = Jrpc.request_with_cancel session request in
    let fire_cancel () =
      let* () = Fiber.Mvar.read request_sent in
      Jrpc.fire cancel
    in
    Fiber.collect_errors (fun () -> Fiber.fork_and_join fire_cancel (fun () -> response))
  in
  let classify = function
    | Ok ((), `Cancelled) -> "cancelled"
    | Error [ _ ] -> "duplicate ID retained"
    | Ok ((), `Ok _) | Error _ -> "unexpected"
  in
  let run () =
    Fiber.fork_and_join_unit
      (fun () -> Jrpc.run session)
      (fun () ->
         let* first = run_request () in
         Printf.printf "first: %s\n" (classify first);
         let* second = run_request () in
         Printf.printf "second: %s\n" (classify second);
         Out.write incoming_writer None)
  in
  Fiber_test.test Dyn.opaque run;
  [%expect
    {|
    first: cancelled
    second: duplicate ID retained
    <opaque> |}]
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
