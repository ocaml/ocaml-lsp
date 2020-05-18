open Import

module In : sig
  type 'a t

  val create : (unit -> 'a option Fiber.t) -> 'a t

  val return : 'a -> 'a t

  val of_list : 'a list -> 'a t

  val read : 'a t -> 'a option Fiber.t
end

module Out : sig
  type 'a t

  val create : ('a option -> unit Fiber.t) -> 'a t

  val write : 'a t -> 'a option -> unit Fiber.t

  val of_ref : 'a list ref -> 'a t

  val null : unit -> 'a t
end
