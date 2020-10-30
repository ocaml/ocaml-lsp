open Stdune
open Fiber_unix.Fiber_stream
open! Jsonrpc
open Jsonrpc_fiber
open Fiber.O

module Stream_chan = struct
  type t = Jsonrpc.packet In.t * Jsonrpc.packet Out.t

  let close (_, o) = Out.write o None

  let send (_, o) p = Out.write o (Some p)

  let recv (i, _) = In.read i
end

module Jrpc = Jsonrpc_fiber.Make (Stream_chan)

let no_output () =
  let received_none = ref false in
  Out.create (function
    | None ->
      if !received_none then
        failwith "received None more than once"
      else
        received_none := true;
      Fiber.return ()
    | Some _ -> failwith "unexpected element")

let%expect_test "start and stop server" =
  let run () =
    let in_ = In.of_list [] in
    let jrpc = Jrpc.create ~name:"test" (in_, no_output ()) () in
    let run = Jrpc.run jrpc in
    Fiber.fork_and_join_unit (fun () -> run) (fun () -> Jrpc.stop jrpc)
  in
  let () = Fiber_test.test Dyn.Encoder.opaque (run ()) in
  [%expect
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | (Failure Fiber.Ivar.fill)
    | Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    | Called from Fiber.Execution_context.apply in file "vendor/fiber/fiber.ml", line 193, characters 9-14
    \-----------------------------------------------------------------------

    [FAIL] unexpected Never raised |}]

let%expect_test "server accepts notifications" =
  let notif =
    { Jsonrpc.Message.id = None
    ; method_ = "method"
    ; params = Some (`String "bar")
    }
  in
  let run () =
    let in_ = In.of_list [ Jsonrpc.Message notif ] in
    let on_notification c =
      let n = Jrpc.Context.message c in
      let state = Jrpc.Context.state c in
      assert (notif = { n with id = None });
      print_endline "received notification";
      Fiber.return (Notify.Stop, state)
    in
    let jrpc =
      Jrpc.create ~name:"test" ~on_notification (in_, no_output ()) ()
    in
    Jrpc.run jrpc
  in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  [%expect {|
    received notification
    "<opaque>" |}]

let%expect_test "stopped fiber" =
  let run () =
    let in_ = In.create (fun () -> Fiber.never) in
    let jrpc = Jrpc.create ~name:"test" (in_, no_output ()) () in
    print_endline "runing";
    let running = Jrpc.run jrpc in
    print_endline "stopping";
    let* () = Jrpc.stop jrpc in
    running
  in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  print_endline "stopped";
  [%expect
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    runing
    stopping
    /-----------------------------------------------------------------------
    | Internal error: Uncaught exception.
    | (Failure "received None more than once")
    | Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
    | Called from Jsonrpc_fiber_tests.no_output.(fun) in file "jsonrpc-fiber/test/jsonrpc_fiber_tests.ml", line 24, characters 8-47
    | Called from Jsonrpc_fiber.Make.close in file "jsonrpc-fiber/src/jsonrpc_fiber.ml", line 89, characters 14-31
    | Called from Fiber.Execution_context.safe_run_k in file "vendor/fiber/fiber.ml", line 127, characters 18-21
    \-----------------------------------------------------------------------

    [FAIL] unexpected Never raised
    stopped |}]

let%expect_test "serving requests" =
  let id = `Int 1 in
  let request =
    { Jsonrpc.Message.id = Some id; method_ = "bla"; params = Some (`Int 100) }
  in
  let response_data = `String "response" in
  let run () =
    let responses = ref [] in
    let in_ = In.of_list [ Jsonrpc.Message request ] in
    let on_request c =
      let r = Jrpc.Context.message c in
      let state = Jrpc.Context.state c in
      assert (r = { request with id = r.id });
      let response = Jsonrpc.Response.ok r.id response_data in
      Fiber.return (response, state)
    in
    let out = Out.of_ref responses in
    let jrpc = Jrpc.create ~name:"test" ~on_request (in_, out) () in
    let+ () = Jrpc.run jrpc in
    List.iter !responses ~f:(fun resp ->
        let json = Jsonrpc.yojson_of_packet resp in
        print_endline (Yojson.Safe.pretty_to_string ~std:false json))
  in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  [%expect
    {|
    { "id": 1, "jsonrpc": "2.0", "result": "response" }
    "<opaque>" |}]

(* The current client/server implement has no concurrent handling of requests.
   We can show this when we try to send a request when handling a response. *)
let%expect_test "concurrent requests" =
  let print_response resp =
    print_endline
      (Yojson.Safe.pretty_to_string ~std:false
         (Jsonrpc.Response.yojson_of_t resp))
  in
  let waiter chan waitee =
    let on_request c =
      print_endline "waiter: received request";
      let counterpart = Jrpc.Context.state c in
      print_endline "waiter: making request";
      let request =
        Jsonrpc.Message.create ~id:(`Int 100) ~method_:"random" ()
      in
      let* response = Jrpc.request counterpart request in
      print_endline "water: received response:";
      print_response response;
      let response =
        let r = Jrpc.Context.message c in
        Jsonrpc.Response.ok r.id `Null
      in
      Fiber.return (response, counterpart)
    in
    Jrpc.create ~name:"waiter" ~on_request chan waitee
  in
  let waitee chan =
    let on_request c =
      print_endline "waitee: received request and returning response";
      let response =
        let r = Jrpc.Context.message c in
        Jsonrpc.Response.ok r.id (`Int 42)
      in
      let state = Jrpc.Context.state c in
      Fiber.return (response, state)
    in
    Jrpc.create ~on_request ~name:"waitee" chan ()
  in
  let waitee_in, waitee_out = pipe () in
  let waitee = waitee (waitee_in, waitee_out) in
  let waiter_in, waiter_out = pipe () in
  let waiter = waiter (waiter_in, waiter_out) waitee in
  let initial_request () =
    let request =
      Jsonrpc.Message.create ~id:(`String "initial") ~method_:"init" ()
    in
    let+ resp = Jrpc.request waitee request in
    print_endline "initial request:";
    print_response resp
  in
  let all = [ Jrpc.run waiter; Jrpc.run waitee ] in
  let all = initial_request () :: all in
  let run () = Fiber.parallel_iter all ~f:Fun.id in
  Fiber_test.test Dyn.Encoder.opaque (run ());
  [%expect
    {|
    waitee: received request and returning response
    initial request:
    { "id": "initial", "jsonrpc": "2.0", "result": 42 }
    [FAIL] unexpected Never raised |}]
