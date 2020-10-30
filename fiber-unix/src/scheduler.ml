open Import

let with_mutex m ~f =
  Mutex.lock m;
  let res = f () in
  Mutex.unlock m;
  res

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
        v
    in
    with_mutex t.m ~f:(fun () -> await_value t)

  let set t v =
    with_mutex t.m ~f:(fun () -> t.cell <- Some v);
    Condition.signal t.cv
end

module Worker : sig
  (** Simple queue that is consumed by its own thread *)
  type 'work t

  val create : do_:('a -> unit) -> 'a t

  type task

  val cancel : task -> unit

  val add_work : 'a t -> 'a -> (task, [ `Stopped ]) result

  val stop : _ t -> unit
end = struct
  type state =
    | Running of Thread.t
    | Stopped of Thread.t
    | Finished

  type 'a t =
    { work : 'a Removable_queue.t
    ; mutable state : state
    ; mutex : Mutex.t
    ; work_available : Condition.t
    }

  and task = Task : 'a t * 'a Removable_queue.node -> task

  let cancel (Task (t, node)) =
    with_mutex t.mutex ~f:(fun () -> Removable_queue.remove node)

  let is_running t =
    match t.state with
    | Running _ -> true
    | Stopped _
    | Finished ->
      false

  let run (f, t) =
    let rec loop () =
      match t.state with
      | Stopped _ -> (
        match Removable_queue.pop t.work with
        | None -> t.state <- Finished
        | Some job -> do_work job )
      | Finished -> ()
      | Running _ -> (
        match Removable_queue.pop t.work with
        | Some job -> do_work job
        | None ->
          while Removable_queue.is_empty t.work && is_running t do
            Condition.wait t.work_available t.mutex
          done;
          loop () )
    and do_work job =
      Mutex.unlock t.mutex;
      f job;
      Mutex.lock t.mutex;
      loop ()
    in
    with_mutex t.mutex ~f:loop

  let create ~do_ =
    let t =
      { work = Removable_queue.create ()
      ; state = Finished
      ; mutex = Mutex.create ()
      ; work_available = Condition.create ()
      }
    in
    t.state <- Running (Thread.create run (do_, t));
    t

  let add_work (type a) (t : a t) (w : a) =
    with_mutex t.mutex ~f:(fun () ->
        if is_running t then (
          let node = Removable_queue.push t.work w in
          Condition.signal t.work_available;
          Ok (Task (t, node))
        ) else
          Error `Stopped)

  let stop (t : _ t) =
    with_mutex t.mutex ~f:(fun () ->
        match t.state with
        | Running th ->
          t.state <- Stopped th;
          Condition.signal t.work_available
        | Stopped _
        | Finished ->
          ())
end

module Timer_id = Id.Make ()

type t =
  { mutable events_pending : int
  ; events : event Queue.t
  ; events_mutex : Mutex.t
  ; time_mutex : Mutex.t
  ; event_ready : Condition.t
  ; timers_available : Condition.t
  ; timers_available_mutex : Mutex.t
  ; mutable threads : thread list
  ; earliest_wakeup : float Mvar.t
  ; mutable time : Thread.t
  ; mutable waker : Thread.t
  ; (* TODO Replace with Removable_queue *)
    timers : (Timer_id.t, active_timer ref) Table.t
  }

and event =
  | Job_completed : 'a * 'a Fiber.Ivar.t -> event
  | Scheduled of active_timer

and job =
  | Pending :
      (unit -> 'a) * ('a, [ `Exn of Exn.t | `Canceled ]) result Fiber.Ivar.t
      -> job

and thread =
  { scheduler : t
  ; worker : job Worker.t
  }

and timer =
  { mutable delay : float
  ; timer_scheduler : t
  ; timer_id : Timer_id.t
  }

and active_timer =
  { scheduled : float
  ; ivar : [ `Resolved | `Cancelled ] Fiber.Ivar.t
  ; parent : timer
  }

let add_events t = function
  | [] -> ()
  | events ->
    with_mutex t.events_mutex ~f:(fun () ->
        List.iter events ~f:(Queue.push t.events);
        Condition.signal t.event_ready)

let is_empty table = Table.length table = 0

let me = Fiber.Var.create ()

let scheduler () = Fiber.Var.get_exn me

let signal_timers_available t =
  with_mutex t.timers_available_mutex ~f:(fun () ->
      Condition.signal t.timers_available)

let time_loop t =
  let rec loop () =
    let to_run = ref [] in
    let earliest_next = ref None in
    with_mutex t.time_mutex ~f:(fun () ->
        if is_empty t.timers then
          ()
        else
          let now = Unix.gettimeofday () in
          Table.filteri_inplace t.timers ~f:(fun ~key:_ ~data:active_timer ->
              let active_timer = !active_timer in
              let scheduled_at =
                active_timer.scheduled +. active_timer.parent.delay
              in
              let need_to_run = scheduled_at < now in
              if need_to_run then
                to_run := Scheduled active_timer :: !to_run
              else
                earliest_next :=
                  Some
                    ( match !earliest_next with
                    | None -> scheduled_at
                    | Some v -> min scheduled_at v );
              not need_to_run));
    add_events t !to_run;
    Option.iter !earliest_next ~f:(Mvar.set t.earliest_wakeup);
    with_mutex t.timers_available_mutex ~f:(fun () ->
        Condition.wait t.timers_available t.timers_available_mutex);
    loop ()
  in
  loop ()

let wake_loop t =
  let rec loop () =
    let wakeup_at = Mvar.get t.earliest_wakeup in
    let now = Unix.gettimeofday () in
    if now < wakeup_at then Thread.delay (wakeup_at -. now);
    signal_timers_available t;
    loop ()
  in
  loop ()

let create () =
  let t =
    { events_pending = 0
    ; events = Queue.create ()
    ; events_mutex = Mutex.create ()
    ; time_mutex = Mutex.create ()
    ; event_ready = Condition.create ()
    ; threads = []
    ; timers = Table.create (module Timer_id) 10
    ; timers_available = Condition.create ()
    ; timers_available_mutex = Mutex.create ()
    ; time = Thread.self ()
    ; earliest_wakeup = Mvar.create ()
    ; waker = Thread.self ()
    }
  in
  t.time <- Thread.create time_loop t;
  t.waker <- Thread.create wake_loop t;
  t

let create_thread scheduler =
  let worker =
    let do_ (Pending (f, ivar)) =
      let res =
        match Result.try_with f with
        | Ok x -> Ok x
        | Error exn -> Error (`Exn exn)
      in
      add_events scheduler [ Job_completed (res, ivar) ]
    in
    Worker.create ~do_
  in
  let t = { scheduler; worker } in
  scheduler.threads <- t :: scheduler.threads;
  t

let add_pending_events t by =
  with_mutex t.events_mutex ~f:(fun () ->
      t.events_pending <- t.events_pending + by;
      assert (t.events_pending >= 0))

type 'a task =
  { ivar : ('a, [ `Exn of Exn.t | `Canceled ]) result Fiber.Ivar.t
  ; task : Worker.task
  }

let await task = Fiber.Ivar.read task.ivar

let await_no_cancel task =
  let open Fiber.O in
  let+ res = Fiber.Ivar.read task.ivar in
  match res with
  | Ok x -> Ok x
  | Error `Canceled -> assert false
  | Error (`Exn exn) -> Error exn

let cancel_task task =
  let open Fiber.O in
  let* status = Fiber.Ivar.peek task.ivar in
  match status with
  | Some _ -> Fiber.return ()
  | None ->
    Worker.cancel task.task;
    Fiber.Ivar.fill task.ivar (Error `Canceled)

let async (t : thread) f =
  add_pending_events t.scheduler 1;
  let ivar = Fiber.Ivar.create () in
  let work = Worker.add_work t.worker (Pending (f, ivar)) in
  Result.map work ~f:(fun task -> { ivar; task })

let async_exn t f =
  match async t f with
  | Error `Stopped -> Code_error.raise "async_exn: stopped thread" []
  | Ok task -> task

let stop (t : thread) = Worker.stop t.worker

let cancel_timers t =
  let timers = ref [] in
  with_mutex t.time_mutex ~f:(fun () ->
      Table.filteri_inplace t.timers ~f:(fun ~key:_ ~data:timer ->
          timers := !timer.ivar :: !timers;
          false));
  Fiber.parallel_iter !timers ~f:(fun ivar -> Fiber.Ivar.fill ivar `Cancelled)

let cleanup t = List.iter t.threads ~f:stop

type run_error = Never

exception Abort of run_error

let event_next (t : t) : Fiber.fill =
  with_mutex t.events_mutex ~f:(fun () ->
      while Queue.is_empty t.events do
        Condition.wait t.event_ready t.events_mutex
      done;
      let consume_event () =
        let res = Queue.pop_exn t.events in
        t.events_pending <- t.events_pending - 1;
        assert (t.events_pending >= 0);
        res
      in
      if Queue.is_empty t.events then
        Error (Abort Never)
      else
        Ok
          ( match consume_event () with
          | Job_completed (a, ivar) -> Fiber.Fill (ivar, a)
          | Scheduled active_timer -> Fill (active_timer.ivar, `Resolved) ))
  |> Result.ok_exn

let report t =
  let status m =
    if Mutex.try_lock m then
      "was unlocked"
    else
      "locked"
  in
  [ ("time_mutex", t.time_mutex)
  ; ("timers_available_mutex", t.timers_available_mutex)
  ; ("events_mutex", t.events_mutex)
  ]
  |> List.iter ~f:(fun (name, mutex) ->
         Format.eprintf "%s: %s@." name (status mutex));
  Format.eprintf "pending events: %d@." t.events_pending;
  Format.eprintf "events: %d@." (Queue.length t.events);
  Format.eprintf "threads: %d@." (List.length t.threads);
  Format.eprintf "timers: %d@." (Table.length t.timers)

let iter (t : t) =
  if t.events_pending = 0 then (
    let () = assert (Queue.is_empty t.events) in
    report t;
    raise (Abort Never)
  ) else
    event_next t

let run : 'a. t -> 'a Fiber.t -> 'a =
 fun t f ->
  let f = Fiber.Var.set me t (fun () -> f) in
  let iter () = iter t in
  let res = Fiber.run f ~iter in
  assert (t.events_pending = 0);
  cleanup t;
  res

let create_timer t ~delay =
  { timer_scheduler = t; delay; timer_id = Timer_id.gen () }

let set_delay t ~delay = t.delay <- delay

let schedule (type a) (timer : timer) (f : unit -> a Fiber.t) :
    (a, [ `Cancelled ]) result Fiber.t =
  let open Fiber.O in
  let active_timer =
    let scheduled = Unix.gettimeofday () in
    { scheduled; ivar = Fiber.Ivar.create (); parent = timer }
  in
  let* () =
    match
      with_mutex timer.timer_scheduler.time_mutex ~f:(fun () ->
          match Table.find timer.timer_scheduler.timers timer.timer_id with
          | Some active ->
            let to_cancel = !active.ivar in
            active := active_timer;
            `Cancel to_cancel
          | None ->
            Table.add_exn timer.timer_scheduler.timers timer.timer_id
              (ref active_timer);
            `Signal_timers_available)
    with
    | `Cancel ivar -> Fiber.Ivar.fill ivar `Cancelled
    | `Signal_timers_available ->
      add_pending_events timer.timer_scheduler 1;
      signal_timers_available timer.timer_scheduler;
      Fiber.return ()
  in
  let* res = Fiber.Ivar.read active_timer.ivar in
  match res with
  | `Cancelled as e -> Fiber.return (Error e)
  | `Resolved ->
    let+ res = f () in
    Ok res

let cancel_timer (timer : timer) =
  let t = timer.timer_scheduler in
  match
    with_mutex t.time_mutex ~f:(fun () ->
        match Table.find t.timers timer.timer_id with
        | None -> None
        | Some at ->
          Table.remove t.timers timer.timer_id;
          Some !at.ivar)
  with
  | None -> Fiber.return ()
  | Some ivar ->
    with_mutex t.events_mutex ~f:(fun () ->
        t.events_pending <- t.events_pending - 1);
    Fiber.Ivar.fill ivar `Cancelled
