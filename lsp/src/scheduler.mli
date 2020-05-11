open Import

type t

val create : unit -> t

val run : t -> 'a Fiber.t -> 'a

type thread

val create_thread : t -> thread

val async : thread -> (unit -> 'a) -> 'a Or_exn.t Fiber.t

val stop : thread -> unit

type timer

val create_timer : t -> delay:float -> timer

val schedule :
  timer -> (unit -> 'a Fiber.t) -> ('a, [ `Cancelled ]) result Fiber.t
