open Stdune
open Fiber_unix.Fiber_stream
open! Jsonrpc

module Stream_chan = struct
  type t = Jsonrpc.packet In.t * Jsonrpc.packet Out.t

  let close (_, o) = Out.write o None

  let send (_, o) p = Out.write o (Some p)

  let recv (i, _) = In.read i
end

module Jrpc = Jsonrpc_fiber.Make (Stream_chan)

let%expect_test "start and stop server" =
  let responses = ref [] in
  let run () =
    let out = Out.of_ref responses in
    let in_ = In.of_list [] in
    let jrpc = Jrpc.create ~name:"test" (in_, out) () in
    let run = Jrpc.run jrpc in
    Fiber.fork_and_join_unit (fun () -> run) (fun () -> Jrpc.stop jrpc)
  in
  let () = Fiber_test.test Dyn.Encoder.opaque (run ()) in
  assert (!responses = []);
  [%expect {|
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
