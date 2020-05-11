open Import

module Worker : sig
  type 'work t
  (** Simple queue that is consumed by its own thread *)

  val create : do_:('a -> unit) -> 'a t

  val add_work : 'a t -> 'a -> unit

  val stop : 'a -> unit
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
    Mutex.lock t.mutex;
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
    t.state <- Running (Thread.create run (do_, t));
    t

  let add_work t w =
    ( match t.state with
    | Running _ -> ()
    | _ -> Code_error.raise "invalid state" [] );
    Mutex.lock t.mutex;
    Queue.add w t.work;
    Condition.signal t.work_available;
    Mutex.unlock t.mutex

  let stop _ = assert false
end

module Timer_id = Id.Make ()

type t =
  { mutable jobs_pending : int
  ; jobs_completed : event Queue.t
  ; mutex : Mutex.t
  ; job_completed : Condition.t
  ; timer_available : Condition.t
  ; mutable threads : thread list
  ; timers : (Timer_id.t, active_timer) Table.t
  }

and event =
  | Job_completed : 'a * 'a Fiber.Ivar.t -> event
  | Scheduled of active_timer

and job = Pending : (unit -> 'a) * 'a Or_exn.t Fiber.Ivar.t -> job

and thread =
  { scheduler : t
  ; jobs : job Queue.t
  ; job_available : Condition.t
  ; thread : Thread.t Lazy.t
  ; th_mutex : Mutex.t
  ; mutable stopped : bool
  }

and timer =
  { delay : float
  ; timer_scheduler : t
  ; timer_id : Timer_id.t
  }

and active_timer =
  { mutable scheduled : float
  ; action : unit -> unit
  ; parent : timer
  }

let create () =
  { jobs_pending = 0
  ; jobs_completed = Queue.create ()
  ; mutex = Mutex.create ()
  ; job_completed = Condition.create ()
  ; threads = []
  ; timer_available = Condition.create ()
  ; timers = Table.create (module Timer_id) 10
  }

let consume_queue q ~f =
  while not (Queue.is_empty q) do
    f (Queue.pop q)
  done

let run_thread (t : thread) =
  Mutex.lock t.th_mutex;
  let rec loop () =
    while Queue.is_empty t.jobs && not t.stopped do
      Condition.wait t.job_available t.th_mutex
    done;
    consume_queue t.jobs ~f:(fun (Pending (f, ivar)) ->
        Mutex.unlock t.th_mutex;
        let res = Result.try_with f in
        Mutex.lock t.scheduler.mutex;
        Queue.add (Job_completed (res, ivar)) t.scheduler.jobs_completed;
        Condition.signal t.scheduler.job_completed;
        Mutex.unlock t.scheduler.mutex);
    if not t.stopped then loop ()
  in
  if not t.stopped then loop ();
  Mutex.unlock t.th_mutex

let create_thread scheduler =
  let rec t : thread =
    { scheduler
    ; thread = lazy (Thread.create run_thread t)
    ; jobs = Queue.create ()
    ; job_available = Condition.create ()
    ; th_mutex = Mutex.create ()
    ; stopped = false
    }
  in
  scheduler.threads <- t :: scheduler.threads;
  t

let async (t : thread) f =
  if t.stopped then
    Code_error.raise "Cannot enqueue jobs after thread is stopped" [];
  let (_ : Thread.t) = Lazy.force t.thread in
  Mutex.lock t.scheduler.mutex;
  t.scheduler.jobs_pending <- t.scheduler.jobs_pending + 1;
  Mutex.unlock t.scheduler.mutex;
  let ivar = Fiber.Ivar.create () in
  Mutex.lock t.th_mutex;
  Queue.add (Pending (f, ivar)) t.jobs;
  Condition.signal t.job_available;
  Mutex.unlock t.th_mutex;
  Fiber.Ivar.read ivar

let stop (t : thread) =
  Mutex.lock t.th_mutex;
  t.stopped <- true;
  Condition.signal t.job_available;
  Mutex.unlock t.th_mutex

let rec pump_events (t : t) =
  let open Fiber.O in
  let* () = Fiber.yield () in
  if t.jobs_pending = 0 then
    Fiber.return (List.iter t.threads ~f:stop)
  else (
    Mutex.lock t.mutex;
    while Queue.is_empty t.jobs_completed do
      Condition.wait t.job_completed t.mutex
    done;
    let* () =
      match Queue.pop t.jobs_completed with
      | Job_completed (a, ivar) ->
        Mutex.unlock t.mutex;
        t.jobs_pending <- t.jobs_pending - 1;
        Fiber.Ivar.fill ivar a
      | Scheduled active_timer ->
        Mutex.unlock t.mutex;
        active_timer.action ();
        Fiber.return ()
    in
    pump_events t
  )

let run t f =
  let open Fiber.O in
  match
    Fiber.run
      (let* user_action_result = Fiber.fork (fun () -> f) in
       let* pump_events_result = pump_events t in
       Fiber.return (pump_events_result, user_action_result))
  with
  | exception Fiber.Never ->
    Code_error.raise "[Scheduler.pump_events] got stuck somehow" []
  | (), b -> Fiber.run (Fiber.Future.wait b)

let create_timer t ~delay =
  { timer_scheduler = t; delay; timer_id = Timer_id.gen () }

let schedule (timer : timer) f =
  Mutex.lock timer.timer_scheduler.mutex;
  let scheduled = Unix.gettimeofday () in
  ( match Table.find timer.timer_scheduler.timers timer.timer_id with
  | Some active -> active.scheduled <- scheduled
  | None ->
    Table.add_exn timer.timer_scheduler.timers timer.timer_id
      { scheduled; action = f; parent = timer } );
  Condition.signal timer.timer_scheduler.timer_available;
  Mutex.unlock timer.timer_scheduler.mutex
