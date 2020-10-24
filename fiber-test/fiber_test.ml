open Stdune

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

  exception Never

  let run t =
    Fiber.run t ~iter:(fun () ->
        match Queue.pop suspended with
        | None -> raise Never
        | Some e -> Fiber.Fill (e, ()))
end

let test ?(expect_never = false) to_dyn f =
  let never_raised = ref false in
  let f =
    let on_error exn =
      Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn
    in
    Fiber.with_error_handler (fun () -> f) ~on_error
  in
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
