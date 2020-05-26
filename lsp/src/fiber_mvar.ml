open Import

type 'a t =
  { writers : ('a * unit Fiber.Ivar.t) Queue.t
  ; readers : 'a Fiber.Ivar.t Queue.t
  ; mutable value : 'a option
  }

let create () =
  { value = None; writers = Queue.create (); readers = Queue.create () }

let try_wakeup t =
  if Queue.is_empty t.readers then
    None
  else
    let reader = Queue.pop t.readers in
    let value =
      let v = Option.value_exn t.value in
      t.value <- None;
      v
    in
    Some (reader, value)

let next_writer (type a) (t : a t) =
  if Queue.is_empty t.writers then
    None
  else
    let a, w = Queue.pop t.writers in
    assert (t.value = None);
    t.value <- Some a;
    Some w

let get (type a) (t : a t) : a Fiber.t =
  match t.value with
  | Some v ->
    t.value <- None;
    let open Fiber.O in
    let+ () =
      match next_writer t with
      | None -> Fiber.return ()
      | Some w ->
        Scheduler.detach (Scheduler.scheduler ()) (fun () ->
            Fiber.Ivar.fill w ())
    in
    v
  | None ->
    let ivar = Fiber.Ivar.create () in
    Queue.add ivar t.readers;
    Fiber.Ivar.read ivar

let set t x =
  match t.value with
  | None -> (
    t.value <- Some x;
    match try_wakeup t with
    | None -> Fiber.return ()
    | Some (reader, value) ->
      Scheduler.detach (Scheduler.scheduler ()) (fun () ->
          Fiber.Ivar.fill reader value) )
  | Some _ ->
    let ivar = Fiber.Ivar.create () in
    Queue.add (x, ivar) t.writers;
    Fiber.Ivar.read ivar
