open Core
open Async

module Fiber = struct
  include Fiber

  include Monad.Make (struct
      include Fiber

      let map = `Custom map
    end)
end

(* Fiber-local storage. [Univ_map.Key] behind the scenes. *)
let (key : (Fiber.fill -> unit) option Fiber.Var.t) = Fiber.Var.create ()

(* This solution is adapted from the [Fiber_lwt] module in Dune. When the fiber scheduler
   reaches a [Fiber.Ivar.read] for an unfilled [Fiber.Ivar.t], it stalls, and when resumed
   it must be give a [Fiber.Fill (ivar, value)] for that ivar so it can enqueue jobs
   waiting on its result. If the [Fiber.Ivar.t] is filled but the fill is not communicated
   to the scheduler, the scheduler will not know the jobs waiting on it are now ready to
   run. Therefore, when we create a [Fiber.Ivar.t] inside [fiber_of_deferred] to hold the
   value when the deferred is filled, we also need some way to communicate with the
   fiber's scheduler. We do this via a pipe that the scheduler provides in fiber-local
   storage, but for that to work the fiber's scheduler must be the one produced by the
   complementary [deferred_of_fiber]. *)

let fiber_of_deferred (type a) (deferred : a Deferred.t) : a Fiber.t =
  let ivar = Fiber.Ivar.create () in
  match%bind.Fiber Fiber.Var.get key with
  | None -> failwith "[fiber_of_deferred] invoked outside of [deferred_of_fiber]"
  | Some fill ->
    match fill with
    | None -> failwith "[fiber_of_deferred] invoked outside of [deferred_of_fiber]"
    | Some fill ->
      upon deferred (fun value -> fill (Fiber.Fill (ivar, value)));
      Fiber.Ivar.read ivar
;;

let deferred_of_fiber fiber () =
  let reader, writer = Pipe.create () in
  let fiber =
    Fiber.Var.set key (Some (Pipe.write_without_pushback writer)) (fun () -> fiber)
  in
  let rec loop witness = function
    | Fiber.Scheduler.Done x -> return x
    | Fiber.Scheduler.Stalled _ ->
      let%bind fill = Pipe.read_exn reader in
      loop witness (Fiber.Scheduler.advance witness [ fill ])
  in
  let step = Fiber.Scheduler.start fiber in
  match step with
  | Done x -> return x
  | Stalled witness -> loop witness step
;;
