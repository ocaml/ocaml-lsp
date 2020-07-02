open Import

type t

val create : unit -> t

val run : t -> 'a Fiber.t -> 'a

type thread

type 'a task

val create_thread : t -> thread

val await : 'a task -> ('a, [ `Exn of Exn.t | `Canceled ]) result Fiber.t

val await_no_cancel : 'a task -> 'a Or_exn.t Fiber.t

val cancel_task : 'a task -> unit Fiber.t

val async : thread -> (unit -> 'a) -> 'a task

val stop : thread -> unit

type timer

val create_timer : t -> delay:float -> timer

val set_delay : timer -> delay:float -> unit

val detach : ?name:string -> t -> (unit -> unit Fiber.t) -> unit Fiber.t

val schedule :
  timer -> (unit -> 'a Fiber.t) -> ('a, [ `Cancelled ]) result Fiber.t

val cancel_timer : timer -> unit

val scheduler : unit -> t

val report : Format.formatter -> t -> unit
