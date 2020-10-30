open Stdune
open Fiber_unix.Fiber_stream
open! Jsonrpc
open Jsonrpc_fiber

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
    | Called from Fiber.Execution_context.exec_in in file "vendor/fiber/fiber.ml", line 110, characters 10-15
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
