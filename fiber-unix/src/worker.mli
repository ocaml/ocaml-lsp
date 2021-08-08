(** Simple queue that is consumed by its own thread *)
type 'work t

val create : do_:('a -> unit) -> 'a t

type task

val cancel : task -> unit

val add_work : 'a t -> 'a -> (task, [ `Stopped ]) result

(** signals the worker to complete tasks currently available in the queue and
    stop. *)
val complete_tasks_and_stop : _ t -> unit
