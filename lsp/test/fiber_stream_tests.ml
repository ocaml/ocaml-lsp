open Lsp
open Stdune
open! Fiber.O
open Dyn.Encoder

let printf = Printf.printf

let print pp = Format.printf "%a@." Pp.render_ignore_tags pp

let print_dyn dyn = print (Dyn.pp dyn)

module Scheduler : sig
  exception Never

  val yield : unit -> unit Fiber.t

  val run : 'a Fiber.t -> 'a
end = struct
  let suspended = Queue.create ()

  let yield () =
    let ivar = Fiber.Ivar.create () in
    Queue.push suspended ivar;
    Fiber.Ivar.read ivar

  let rec restart_suspended () =
    if Queue.is_empty suspended then
      Fiber.return ()
    else
      let* () = Fiber.Ivar.fill (Queue.pop_exn suspended) () in
      restart_suspended ()

  exception Never

  let run t =
    match
      Fiber.run
        (let* result = Fiber.fork (fun () -> t) in
         let* () = restart_suspended () in
         Fiber.Future.peek result)
    with
    | None
    | Some None ->
      raise Never
    | Some (Some x) -> x
end

let failing_fiber () : unit Fiber.t =
  Scheduler.yield () >>= fun () -> raise Exit

let long_running_fiber () =
  let rec loop n =
    if n = 0 then
      Fiber.return ()
    else
      Scheduler.yield () >>= fun () -> loop (n - 1)
  in
  loop 10

let test ?(expect_never = false) to_dyn f =
  let never_raised = ref false in
  ( try Scheduler.run f |> to_dyn |> print_dyn
    with Scheduler.Never -> never_raised := true );
  match (!never_raised, expect_never) with
  | false, false ->
    (* We don't raise in this case b/c we assume something else is being tested *)
    ()
  | true, true -> print_endline "[PASS] Never raised as expected"
  | false, true ->
    print_endline "[FAIL] expected Never to be raised but it wasn't"
  | true, false -> print_endline "[FAIL] unexpected Never raised"

open Fiber_stream

let%expect_test "of_list & iter" =
  test unit
    (let i = In.of_list [ 1; 2; 3 ] in
     In.sequential_iter i ~f:(fun i -> Fiber.return (print_int i)));
  [%expect {| 123() |}]

let%expect_test "pipe" =
  test unit
    (let i, o = Fiber_stream.pipe () in
     Fiber.fork_and_join_unit
       (fun () ->
         let* () =
           Fiber.sequential_iter [ 1; 2; 3 ] ~f:(fun i ->
               printf "writing %d\n" i;
               Out.write o (Some i))
         in
         print_endline "closing stream";
         Out.write o None)
       (fun () ->
         let* () =
           In.sequential_iter i ~f:(fun i ->
               printf "reading %d\n" i;
               Fiber.return ())
         in
         print_endline "finished reading";
         Fiber.return ()));
  [%expect
    {|
    writing 1
    writing 2
    reading 1
    reading 2
    writing 3
    closing stream
    reading 3
    finished reading
    () |}]

let writer f =
  Out.create (function
    | None ->
      print_endline "closed out";
      Fiber.return ()
    | Some x ->
      printf "writing %s\n" (f x);
      Fiber.return ())

let%expect_test "connect" =
  test unit
    (let i = In.of_list [ 1; 2; 3 ] in
     let o = writer string_of_int in
     let+ () = Fiber_stream.connect i o in
     print_endline "connect finished");
  [%expect
    {|
    writing 1
    writing 2
    writing 3
    closed out
    connect finished
    () |}]

let%expect_test "supply" =
  test unit
    (let i = In.of_list [ 1; 2; 3 ] in
     let o = writer string_of_int in
     let* () = Fiber_stream.supply i o in
     print_endline "suply finished, but not closed. manually closing";
     Out.write o None);
  [%expect
    {|
    writing 1
    writing 2
    writing 3
    suply finished, but not closed. manually closing
    closed out
    () |}]
