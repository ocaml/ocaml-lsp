open! Import
include module type of Types.Position with type t = Types.Position.t

val zero : t
val is_zero : t -> bool
val compare : t -> t -> int
val min : t -> t -> t
val max : t -> t -> t
