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
        Mutex.unlock t.m;
        v
    in
    Mutex.lock t.m;
    await_value t

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

  val add_work : 'a t -> 'a -> task

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
    | _ -> false

  let run (f, t) =
    let rec loop () =
      while Removable_queue.is_empty t.work && is_running t do
        Condition.wait t.work_available t.mutex
      done;
      while not (Removable_queue.is_empty t.work) do
        f (Option.value_exn (Removable_queue.pop t.work))
      done;
      match t.state with
      | Stopped _ ->
        assert (Removable_queue.is_empty t.work);
        t.state <- Finished
      | Finished -> assert false
      | Running _ -> loop ()
    in
    loop ()

  let create ~do_ =
    let t =
      { work = Removable_queue.create ()
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

  let add_work (type a) (t : a t) (w : a) =
    ( match t.state with
    | Running _ -> ()
    | _ -> Code_error.raise "invalid state" [] );
    with_mutex t.mutex ~f:(fun () ->
        let node = Removable_queue.push t.work w in
        Condition.signal t.work_available;
        Task (t, node))

  let stop (t : _ t) =
    match t.state with
    | Running th -> t.state <- Stopped th
    | _ -> ()
end

module Timer_id = Id.Make ()

type detached_task =
  { name : string option
  ; task : unit -> unit Fiber.t
  }

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
  ; detached : detached_task Queue.t
  }

and event =
  | Job_completed : 'a * 'a Fiber.Ivar.t -> event
  | Scheduled of packed_active_timer
  | Detached of detached_task

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

and 'a active_timer =
  { scheduled : float
  ; ivar : ('a, [ `Cancelled ]) result Fiber.Ivar.t
  ; action : unit -> 'a Fiber.t
  ; parent : timer
  }

and packed_active_timer =
  | Active_timer : 'a active_timer -> packed_active_timer

let dyn_event_tag : event -> Dyn.t =
  let open Dyn.Encoder in
  function
  | Job_completed (_, _) -> constr "Job_completed" []
  | Scheduled _ -> constr "Scheduled" []
  | Detached _ -> constr "Detached" []

let add_events t = function
  | [] -> ()
  | events ->
    with_mutex t.mutex ~f:(fun () ->
        t.events_pending <- t.events_pending - List.length events;
        List.iter events ~f:(Queue.push t.events);
        Condition.signal t.event_ready)

let is_empty table = Table.length table = 0

let me = Fiber.Var.create ()

let scheduler () = Fiber.Var.get_exn me

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
    ; detached = Queue.create ()
    }
  in
  Mutex.lock t.time_mutex;
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
  with_mutex t.mutex ~f:(fun () -> t.events_pending <- t.events_pending + by)

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
  let task = Worker.add_work t.worker (Pending (f, ivar)) in
  { ivar; task }

let stop (t : thread) = Worker.stop t.worker

let log = Log.log ~section:"scheduler"

let assoc_of_name = function
  | None -> []
  | Some name -> [ ("name", `String name) ]

let rec pump_events (t : t) =
  let open Fiber.O in
  if t.events_pending = 0 && Queue.is_empty t.events then (
    log (fun () -> Log.msg "finished processing events" []);
    Fiber.return (List.iter t.threads ~f:stop)
  ) else if t.events_pending < 0 then
    assert false
  else (
    Mutex.lock t.mutex;
    while Queue.is_empty t.events do
      Condition.wait t.event_ready t.mutex
    done;
    let consume_event () =
      let res = Queue.pop_exn t.events in
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
          | Detached { name; task } ->
            log (fun () ->
                let args = assoc_of_name name in
                Log.msg "running detached task" args);
            let task () =
              let+ () = task () in
              log (fun () ->
                  let args = assoc_of_name name in
                  Log.msg "finished detached task" args)
            in
            let+ (_ : unit Fiber.Future.t) =
              Fiber.fork (fun () -> Fiber.Var.set me t task)
            in
            ()
          | Job_completed (a, ivar) -> (
            let* status = Fiber.Ivar.peek ivar in
            (* in cases where a canceled task was already running *)
            match status with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill ivar a )
          | Scheduled (Active_timer active_timer) ->
            with_mutex t.time_mutex ~f:(fun () ->
                Table.remove t.timers active_timer.parent.timer_id);
            let+ (_ : _ Fiber.Future.t) =
              Fiber.fork (fun () ->
                  let* res = active_timer.action () in
                  Fiber.Ivar.fill active_timer.ivar (Ok res))
            in
            ()
        in
        Mutex.lock t.mutex;
        aux ()
    in
    let* () = aux () in
    pump_events t
  )

exception Never

(* This implementation of dequeing detached tasks is buggy. What happens when we
   detach that rely on some subsequent detached task to proceed?

   The issue is that we should wait for more tasks in parallel *)
let rec restart_suspended t =
  if Queue.is_empty t.detached then (
    log (fun () -> Log.msg "finished processing detached tasks" []);
    Fiber.return ()
  ) else
    let open Fiber.O in
    let* () =
      let detached =
        Queue.fold ~f:(fun acc a -> a :: acc) ~init:[] t.detached |> List.rev
      in
      log (fun () ->
          Log.msg
            ( "cleared "
            ^ Int.to_string (Queue.length t.detached)
            ^ " detached tasks" )
            []);
      Queue.clear t.detached;
      Fiber.parallel_iter
        ~f:(fun f ->
          log (fun () ->
              let args = assoc_of_name f.name in
              Log.msg "running detached task" args);
          f.task ())
        detached
    in
    restart_suspended t

let list_of_queue q =
  List.rev (Queue.fold ~f:(fun acc a -> a :: acc) ~init:[] q)

let run : 'a. t -> 'a Fiber.t -> 'a =
 fun t f ->
  let f = Fiber.Var.set me t (fun () -> f) in
  match
    let fiber =
      let open Fiber.O in
      let* user_action = Fiber.fork (fun () -> f) in
      let* (_ : unit Fiber.Future.t) =
        Fiber.fork (fun () -> restart_suspended t)
      in
      let* () = pump_events t in
      Fiber.Future.peek user_action
    in
    Fiber.run fiber
  with
  | None
  | Some None ->
    if not (Queue.is_empty t.detached) then
      Code_error.raise "detached tasks left" [];
    if t.events_pending <> 0 || not (Queue.is_empty t.events) then
      Code_error.raise "pending events ignored"
        [ ("events_pending", Int t.events_pending)
        ; ("events", Dyn.Encoder.list dyn_event_tag (list_of_queue t.events))
        ];
    raise Never
  | Some (Some x) -> x

let create_timer t ~delay =
  { timer_scheduler = t; delay; timer_id = Timer_id.gen () }

let set_delay t ~delay = t.delay <- delay

let schedule (type a) (timer : timer) (f : unit -> a Fiber.t) :
    (a, [ `Cancelled ]) result Fiber.t =
  let scheduled = Unix.gettimeofday () in
  let make_active_timer action =
    { scheduled; action; ivar = Fiber.Ivar.create (); parent = timer }
  in
  let ivar =
    with_mutex timer.timer_scheduler.time_mutex ~f:(fun () ->
        match Table.find timer.timer_scheduler.timers timer.timer_id with
        | Some active ->
          let fill_old =
            let (Active_timer active) = !active in
            log (fun () -> Log.msg "cancelled task" []);
            Fiber.Ivar.fill active.ivar (Error `Cancelled)
          in
          let action () = Fiber.fork_and_join_unit (fun () -> fill_old) f in
          let active_timer = make_active_timer action in
          active := Active_timer active_timer;
          active_timer.ivar
        | None ->
          add_pending_events timer.timer_scheduler 1;
          let active_timer = make_active_timer f in
          Table.add_exn timer.timer_scheduler.timers timer.timer_id
            (ref (Active_timer active_timer));
          Condition.signal timer.timer_scheduler.timers_available;
          active_timer.ivar)
  in
  Fiber.Ivar.read ivar

let cancel_timer (timer : timer) =
  with_mutex timer.timer_scheduler.time_mutex ~f:(fun () ->
      Table.remove timer.timer_scheduler.timers timer.timer_id)

let detach ?name t f =
  let task () =
    Fiber.with_error_handler
      ~on_error:(fun e ->
        Format.eprintf "detached raised:@.%s@.%!"
          (Dyn.to_string (Exn_with_backtrace.to_dyn e)))
      (fun () -> Fiber.map (f ()) ~f:ignore)
  in
  let event = Detached { name; task } in
  with_mutex t.mutex ~f:(fun () -> Queue.push t.events event);
  Fiber.return ()

let report ppf t =
  Format.fprintf ppf "detached tasks:@.";
  Queue.iter t.detached ~f:(fun (dt : detached_task) ->
      let name = Option.value ~default:"<Anon>" dt.name in
      Format.fprintf ppf "- %s@." name)
