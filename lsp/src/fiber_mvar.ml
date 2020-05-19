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
    Fiber.return ()
  else
    let reader = Queue.pop t.readers in
    let value =
      let v = Option.value_exn t.value in
      t.value <- None;
      v
    in
    Fiber.Ivar.fill reader value

let next_writer (type a) (t : a t) : unit Fiber.t =
  if Queue.is_empty t.writers then
    Fiber.return ()
  else
    let a, w = Queue.pop t.writers in
    assert (t.value = None);
    t.value <- Some a;
    Fiber.Ivar.fill w ()

let get (type a) (t : a t) : a Fiber.t =
  match t.value with
  | Some v ->
    t.value <- None;
    Scheduler.detach (Scheduler.scheduler ()) (next_writer t);
    Fiber.return v
  | None ->
    let ivar = Fiber.Ivar.create () in
    Queue.add ivar t.readers;
    Fiber.Ivar.read ivar

let set t x =
  match t.value with
  | None ->
    t.value <- Some x;
    Scheduler.detach (Scheduler.scheduler ()) (try_wakeup t);
    Fiber.return ()
  | Some _ ->
    let ivar = Fiber.Ivar.create () in
    Queue.add (x, ivar) t.writers;
    Fiber.Ivar.read ivar
