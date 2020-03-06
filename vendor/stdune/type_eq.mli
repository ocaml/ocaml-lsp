(** Equality between types. See [Base.Type_equal] for documentation. *)

type ('a, 'b) t = T : ('a, 'a) t

val cast : ('a, 'b) t -> 'a -> 'b

(** [Id] provides identifiers for types, and the ability to test (via [Id.same])
    at runtime if two identifiers are equal, and if so to get a proof of
    equality of their types. *)
module Id : sig
  type ('a, 'b) eq

  type 'a t

  val create : unit -> 'a t

  val same : 'a t -> 'b t -> ('a, 'b) eq option
end
with type ('a, 'b) eq := ('a, 'b) t
