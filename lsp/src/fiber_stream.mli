open Import

module In : sig
  type 'a t

  val create : (unit -> 'a option Fiber.t) -> 'a t

  val empty : unit -> 'a t

  val of_list : 'a list -> 'a t

  val read : 'a t -> 'a option Fiber.t

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

module Out : sig
  type 'a t

  val create : ('a option -> unit Fiber.t) -> 'a t

  val write : 'a t -> 'a option -> unit Fiber.t

  val of_ref : 'a list ref -> 'a t

  val null : unit -> 'a t
end

val connect : 'a In.t -> 'a Out.t -> 'a Fiber.t

val pipe : unit -> 'a In.t * 'a Out.t
