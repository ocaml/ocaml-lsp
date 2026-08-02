open! Import
include module type of Types.Position with type t = Types.Position.t

val zero : t
val is_zero : t -> bool
val compare : t -> t -> int
val min : t -> t -> t
val max : t -> t -> t

(** [to_logical t] converts the 0-based position to a 1-based line
    convention: [(line + 1, character)]. *)
val to_logical : t -> int * int

(** [of_logical ~line ~character] converts a position with a 1-based line and a
    0-based character back to a regular position. *)
val of_logical : line:int -> character:int -> t
