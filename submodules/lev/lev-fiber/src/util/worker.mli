type 'work t
(** Simple queue that is consumed by its own thread *)

val create :
  spawn_thread:((unit -> unit) -> Thread.t) -> do_no_raise:('a -> unit) -> 'a t
(** [create ~spawn_thread ~do_no_raise] creates a worker with a task handler
    [do_no_raise]. The worker will not handle an exception raised by the task
    handler, so [do_no_raise] is expected to not raise. [spawn_thread] is used
    to launch the thread doing the work *)

type task

val cancel_if_not_consumed : task -> unit
(** Cancels the task in the queue if it hasn't already been consumed by the
    thread. Does nothing if the task has been consumed already. *)

val add_work : 'a t -> 'a -> (task, [ `Stopped ]) result

module Id : sig
  type t

  val equal : t -> t -> bool
end

val id : _ t -> Id.t
val join : _ t -> unit

val complete_tasks_and_stop : _ t -> unit
(** Signals the worker to complete tasks currently available in the queue and
    stop. *)
