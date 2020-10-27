open Import

type mvar =
  | Done
  | Task of (unit -> unit Fiber.t)

type t =
  { mvar : mvar Fiber.Mvar.t
  ; mutable closed : bool
  }

let create () = { mvar = Fiber.Mvar.create (); closed = false }

let task t ~f = Fiber.Mvar.write t.mvar (Task f)

let rec sequence t =
  let open Fiber.O in
  let+ next = Fiber.Mvar.read t.mvar in
  match next with
  | Done -> Fiber.Sequence.Nil
  | Task task -> Cons (task, sequence t)

let check_open t = if t.closed then Code_error.raise "detached already open" []

let stop t =
  check_open t;
  let open Fiber.O in
  let+ () = Fiber.Mvar.write t.mvar Done in
  check_open t;
  t.closed <- true

let run t = sequence t |> Fiber.Sequence.parallel_iter ~f:(fun task -> task ())
