open Import

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
      | Some job -> do_work job)
    | Finished -> ()
    | Running _ -> (
      match Removable_queue.pop t.work with
      | Some job -> do_work job
      | None ->
        while Removable_queue.is_empty t.work && is_running t do
          Condition.wait t.work_available t.mutex
        done;
        loop ())
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

let complete_tasks_and_stop (t : _ t) =
  with_mutex t.mutex ~f:(fun () ->
      match t.state with
      | Running th ->
        t.state <- Stopped th;
        Condition.signal t.work_available
      | Stopped _
      | Finished ->
        ())
