open Import

type 'a t =
  { value : 'a Fiber.Ivar.t
  ; mutable f : (unit -> 'a Fiber.t) option
  }

let create f = { f = Some f; value = Fiber.Ivar.create () }

let force t =
  let open Fiber.O in
  let* v = Fiber.Ivar.peek t.value in
  match v with
  | Some s -> Fiber.return s
  | None ->
    let* v = (Option.value_exn t.f) () in
    let+ () = Fiber.Ivar.fill t.value v in
    t.f <- None;
    v
