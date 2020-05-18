type 'a t

val create : unit -> 'a t

val get : 'a t -> 'a Fiber.t

val set : 'a t -> 'a -> unit Fiber.t
