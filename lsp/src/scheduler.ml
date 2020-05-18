open Import

module Mvar : sig
  type 'a t

  val create : unit -> 'a t

  val get : 'a t -> 'a

  val set : 'a t -> 'a -> unit
end = struct
  type 'a t =
    { m : Mutex.t
    ; cv : Condition.t
    ; mutable cell : 'a option
    }

  let create () = { m = Mutex.create (); cv = Condition.create (); cell = None }

  let get t =
    let rec await_value t =
      match t.cell with
      | None ->
        Condition.wait t.cv t.m;
        await_value t
      | Some v ->
        t.cell <- None;
        Mutex.unlock t.m;
        v
    in
    Mutex.lock t.m;
    await_value t

  let set t v =
    Mutex.lock t.m;
    t.cell <- Some v;
    Mutex.unlock t.m;
    Condition.signal t.cv
end

module Worker : sig
  (** Simple queue that is consumed by its own thread *)
  type 'work t

  val create : do_:('a -> unit) -> 'a t

  val add_work : 'a t -> 'a -> unit

  val stop : _ t -> unit
end = struct
  type state =
    | Running of Thread.t
    | Stopped of Thread.t
    | Finished

  type 'a t =
    { work : 'a Queue.t
    ; mutable state : state
    ; mutex : Mutex.t
    ; work_available : Condition.t
    }

  let is_running t =
    match t.state with
    | Running _ -> true
    | _ -> false

  let run (f, t) =
    let rec loop () =
      while Queue.is_empty t.work && is_running t do
        Condition.wait t.work_available t.mutex
      done;
      while not (Queue.is_empty t.work) do
        f (Queue.pop t.work)
      done;
      match t.state with
      | Stopped _ ->
        assert (Queue.is_empty t.work);
        t.state <- Finished
      | Finished -> assert false
      | Running _ -> loop ()
    in
    loop ()

  let create ~do_ =
    let t =
      { work = Queue.create ()
      ; state = Finished
      ; mutex = Mutex.create ()
      ; work_available = Condition.create ()
      }
    in
    Mutex.lock t.mutex;
    t.state <- Running (Thread.create run (do_, t));
    Mutex.lock t.mutex;
    Mutex.unlock t.mutex;
    t

  let add_work t w =
    ( match t.state with
    | Running _ -> ()
    | _ -> Code_error.raise "invalid state" [] );
    Mutex.lock t.mutex;
    Queue.add w t.work;
    Condition.signal t.work_available;
    Mutex.unlock t.mutex

  let stop (t : _ t) =
    match t.state with
    | Running th -> t.state <- Stopped th
    | _ -> ()
end

module Timer_id = Id.Make ()

type t =
  { mutable events_pending : int
  ; events : event Queue.t
  ; mutex : Mutex.t
  ; time_mutex : Mutex.t
  ; event_ready : Condition.t
  ; timers_available : Condition.t
  ; mutable threads : thread list
  ; earliest_wakeup : float Mvar.t
  ; mutable time : Thread.t
  ; mutable waker : Thread.t
  ; timers : (Timer_id.t, packed_active_timer ref) Table.t
  }

and event =
  | Job_completed : 'a * 'a Fiber.Ivar.t -> event
  | Scheduled of packed_active_timer

and job = Pending : (unit -> 'a) * 'a Or_exn.t Fiber.Ivar.t -> job

and thread =
  { scheduler : t
  ; worker : job Worker.t
  }

and timer =
  { delay : float
  ; timer_scheduler : t
  ; timer_id : Timer_id.t
  }

and 'a active_timer =
  { scheduled : float
  ; ivar : ('a, [ `Cancelled ]) result Fiber.Ivar.t
  ; action : unit -> 'a Fiber.t
  ; parent : timer
  }

and packed_active_timer =
  | Active_timer : 'a active_timer -> packed_active_timer

let add_events t = function
  | [] -> ()
  | events ->
    Mutex.lock t.mutex;
    t.events_pending <- t.events_pending - List.length events;
    List.iter events ~f:(fun e -> Queue.add e t.events);
    Condition.signal t.event_ready;
    Mutex.unlock t.mutex

let is_empty table = Table.length table = 0

let time_loop t =
  let rec loop () =
    Condition.wait t.timers_available t.time_mutex;
    if is_empty t.timers then
      loop ()
    else
      let to_run = ref [] in
      let now = Unix.gettimeofday () in
      let earliest_next = ref None in
      Table.filteri_inplace t.timers ~f:(fun ~key:_ ~data:active_timer ->
          let (Active_timer active_timer) = !active_timer in
          let scheduled_at =
            active_timer.scheduled +. active_timer.parent.delay
          in
          let need_to_run = scheduled_at < now in
          if need_to_run then
            to_run := Scheduled (Active_timer active_timer) :: !to_run
          else
            earliest_next :=
              Some
                ( match !earliest_next with
                | None -> scheduled_at
                | Some v -> min scheduled_at v );
          not need_to_run);
      Option.iter !earliest_next ~f:(Mvar.set t.earliest_wakeup);
      add_events t !to_run;
      loop ()
  in
  loop ()

let wake_loop t =
  let rec loop () =
    let wakeup_at = Mvar.get t.earliest_wakeup in
    let now = Unix.gettimeofday () in
    if now < wakeup_at then Thread.delay (wakeup_at -. now);
    Condition.signal t.timers_available;
    loop ()
  in

  loop ()

let create () =
  let t =
    { events_pending = 0
    ; events = Queue.create ()
    ; mutex = Mutex.create ()
    ; time_mutex = Mutex.create ()
    ; event_ready = Condition.create ()
    ; threads = []
    ; timers = Table.create (module Timer_id) 10
    ; timers_available = Condition.create ()
    ; time = Thread.self ()
    ; earliest_wakeup = Mvar.create ()
    ; waker = Thread.self ()
    }
  in
  Mutex.lock t.time_mutex;
  t.time <- Thread.create time_loop t;
  t.waker <- Thread.create wake_loop t;
  t

let create_thread scheduler =
  let worker =
    let do_ (Pending (f, ivar)) =
      let res = Result.try_with f in
      add_events scheduler [ Job_completed (res, ivar) ]
    in
    Worker.create ~do_
  in
  let t = { scheduler; worker } in
  scheduler.threads <- t :: scheduler.threads;
  t

let add_pending_events t by =
  Mutex.lock t.mutex;
  t.events_pending <- t.events_pending + by;
  Mutex.unlock t.mutex

let async (t : thread) f =
  add_pending_events t.scheduler 1;
  let ivar = Fiber.Ivar.create () in
  Worker.add_work t.worker (Pending (f, ivar));
  Fiber.Ivar.read ivar

let stop (t : thread) = Worker.stop t.worker

let rec pump_events (t : t) =
  let open Fiber.O in
  if t.events_pending = 0 && Queue.is_empty t.events then
    Fiber.return (List.iter t.threads ~f:stop)
  else if t.events_pending < 0 then
    assert false
  else (
    Mutex.lock t.mutex;
    while Queue.is_empty t.events do
      Condition.wait t.event_ready t.mutex
    done;
    let consume_event () =
      let res = Queue.pop t.events in
      Mutex.unlock t.mutex;
      res
    in
    let rec aux () =
      if Queue.is_empty t.events then (
        Mutex.unlock t.mutex;
        Fiber.return ()
      ) else
        let* () =
          match consume_event () with
          | Job_completed (a, ivar) -> Fiber.Ivar.fill ivar a
          | Scheduled (Active_timer active_timer) ->
            Table.remove t.timers active_timer.parent.timer_id;
            Mutex.unlock t.time_mutex;
            let* res = active_timer.action () in
            Fiber.Ivar.fill active_timer.ivar (Ok res)
        in
        Mutex.lock t.mutex;
        aux ()
    in
    let* () = aux () in
    pump_events t
  )

exception Never

let run t f =
  let open Fiber.O in
  match
    Fiber.run
      (let* user_action = Fiber.fork (fun () -> f) in
       let* () = pump_events t in
       Fiber.Future.peek user_action)
  with
  | None
  | Some None ->
    raise Never
  | Some (Some x) -> x

let create_timer t ~delay =
  { timer_scheduler = t; delay; timer_id = Timer_id.gen () }

let schedule (type a) (timer : timer) (f : unit -> a Fiber.t) :
    (a, [ `Cancelled ]) result Fiber.t =
  let scheduled = Unix.gettimeofday () in
  Mutex.lock timer.timer_scheduler.time_mutex;
  let ivar =
    match Table.find timer.timer_scheduler.timers timer.timer_id with
    | Some active ->
      let fill_old =
        let (Active_timer active) = !active in
        Fiber.Ivar.fill active.ivar (Error `Cancelled)
      in
      let ivar = Fiber.Ivar.create () in
      let action () = Fiber.fork_and_join_unit (fun () -> fill_old) f in
      active := Active_timer { scheduled; action; ivar; parent = timer };
      ivar
    | None ->
      add_pending_events timer.timer_scheduler 1;
      let ivar = Fiber.Ivar.create () in
      Table.add_exn timer.timer_scheduler.timers timer.timer_id
        (ref (Active_timer { scheduled; action = f; ivar; parent = timer }));
      Condition.signal timer.timer_scheduler.timers_available;
      ivar
  in
  Mutex.unlock timer.timer_scheduler.time_mutex;
  Fiber.Ivar.read ivar
