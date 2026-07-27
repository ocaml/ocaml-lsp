type 'a result = ('a, Stdune.Exn_with_backtrace.t list) Result.t

type 'a t =
  { value : 'a result Fiber.Ivar.t
  ; mutable f : (unit -> 'a Fiber.t) option
  }

let create f = { f = Some f; value = Fiber.Ivar.create () }

let await t =
  let open Fiber.O in
  Fiber.Ivar.read t.value
  >>= function
  | Ok value -> Fiber.return value
  | Error errors -> Fiber.reraise_all errors
;;

let force t =
  let open Fiber.O in
  Fiber.of_thunk (fun () ->
    match t.f with
    | None -> await t
    | Some f ->
      t.f <- None;
      let* result = Fiber.collect_errors f in
      let* () = Fiber.Ivar.fill t.value result in
      (match result with
       | Ok value -> Fiber.return value
       | Error errors -> Fiber.reraise_all errors))
;;
