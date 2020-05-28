open Import

module In : sig
  type 'a t

  val create : (unit -> 'a option Fiber.t) -> 'a t

  val empty : unit -> 'a t

  val of_list : 'a list -> 'a t

  val read : 'a t -> 'a option Fiber.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val filter : 'a t -> f:('a -> bool) -> 'a t

  val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
end

module Out : sig
  type 'a t

  val create : ('a option -> unit Fiber.t) -> 'a t

  val write : 'a t -> 'a option -> unit Fiber.t

  val of_ref : 'a list ref -> 'a t

  val null : unit -> 'a t
end

(** [connect i o] read from [i] and write them to [o]. Close [o] when [i] is
    exhausted *)
val connect : 'a In.t -> 'a Out.t -> 'a Fiber.t

(** [supply i o] like [connect i o] but does not close [o] once [i] is exhausted *)
val supply : 'a In.t -> 'a Out.t -> 'a Fiber.t

val pipe : unit -> 'a In.t * 'a Out.t
