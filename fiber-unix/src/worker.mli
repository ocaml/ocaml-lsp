(** Simple queue that is consumed by its own thread *)
type 'work t

val create : do_:('a -> unit) -> 'a t

type task

(** Cancels the task in the queue if it hasn't already been consumed by the
    thread. Does nothing if the task has been consumed already. *)
val cancel_if_not_consumed : task -> unit

val add_work : 'a t -> 'a -> (task, [ `Stopped ]) result

(** Signals the worker to complete tasks currently available in the queue and
    stop. *)
val complete_tasks_and_stop : _ t -> unit
