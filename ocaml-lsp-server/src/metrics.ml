open Import
open Fiber.O

let max_len = 1_000

type t = { events : Chrome_trace.Event.t Queue.t }

let t_var : t Fiber.Var.t = Fiber.Var.create ()
let get () = Fiber.Var.get_exn t_var

let report (event : Chrome_trace.Event.t) : unit Fiber.t =
  let+ t = get () in
  if Queue.length t.events >= max_len
  then (
    let (_ : Chrome_trace.Event.t) = Queue.dequeue_exn t.events in
    ());
  Queue.enqueue t.events event
;;

let dump () =
  let+ t = get () in
  let traceEvents = Queue.to_list t.events in
  let json =
    Chrome_trace.Output_object.create ~traceEvents ()
    |> Chrome_trace.Output_object.to_json
  in
  Json.to_string (json :> Json.t)
;;

let create () = { events = Queue.create () }
let with_metrics t f = Fiber.Var.set t_var t f
