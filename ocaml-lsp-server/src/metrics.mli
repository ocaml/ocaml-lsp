type t

val create : unit -> t
val with_metrics : t -> (unit -> 'a Fiber.t) -> 'a Fiber.t
val report : Chrome_trace.Event.t -> unit Fiber.t
val dump : unit -> string Fiber.t
