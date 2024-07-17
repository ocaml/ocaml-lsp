open Stdune

let print pp = Format.printf "%a@." Pp.to_fmt pp
let print_dyn dyn = print (Dyn.pp dyn)

module Scheduler : sig
  type t

  exception Never

  val create : unit -> t
  val run : t -> 'a Fiber.t -> 'a
end = struct
  type t = unit Fiber.Ivar.t Queue.t

  let t_var = Fiber.Var.create ()
  let create () = Queue.create ()

  exception Never

  let run t fiber =
    let fiber = Fiber.Var.set t_var t (fun () -> fiber) in
    Fiber.run fiber ~iter:(fun () ->
      let next =
        match Queue.pop t with
        | None -> raise Never
        | Some e -> Fiber.Fill (e, ())
      in
      Nonempty_list.[ next ])
  ;;
end

let test ?(expect_never = false) to_dyn f =
  let never_raised = ref false in
  let f =
    let on_error exn =
      Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
      Exn_with_backtrace.reraise exn
    in
    Fiber.with_error_handler f ~on_error
  in
  (try Scheduler.run (Scheduler.create ()) f |> to_dyn |> print_dyn with
   | Scheduler.Never -> never_raised := true);
  match !never_raised, expect_never with
  | false, false ->
    (* We don't raise in this case b/c we assume something else is being
       tested *)
    ()
  | true, true -> print_endline "[PASS] Never raised as expected"
  | false, true -> print_endline "[FAIL] expected Never to be raised but it wasn't"
  | true, false -> print_endline "[FAIL] unexpected Never raised"
;;
