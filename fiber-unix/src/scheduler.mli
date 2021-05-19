open Import

type run_error =
  | Never
  | Abort_requested
  | Exn of Exn_with_backtrace.t

val run_result : 'a Fiber.t -> ('a, run_error) result

val run : 'a Fiber.t -> 'a

type thread

type 'a task

val create_thread : unit -> thread Fiber.t

val await :
  'a task -> ('a, [ `Exn of Exn_with_backtrace.t | `Canceled ]) result Fiber.t

val await_no_cancel : 'a task -> ('a, Exn_with_backtrace.t) result Fiber.t

val cancel_task : 'a task -> unit Fiber.t

val async : thread -> (unit -> 'a) -> ('a task, [ `Stopped ]) result

val async_exn : thread -> (unit -> 'a) -> 'a task

val stop : thread -> unit

type timer

val create_timer : delay:float -> timer Fiber.t

val set_delay : timer -> delay:float -> unit

val schedule :
  timer -> (unit -> 'a Fiber.t) -> ('a, [ `Cancelled ]) result Fiber.t

val cancel_timer : timer -> unit Fiber.t

val cancel_timers : unit -> unit Fiber.t

val wait_for_process : Pid.t -> Unix.process_status Fiber.t

val abort : unit -> unit Fiber.t
