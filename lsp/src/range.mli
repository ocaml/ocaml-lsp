open! Import
include module type of Types.Range with type t = Types.Range.t

(** Compare ranges by start position, then by end position. *)
val compare : t -> t -> int

(** [contains outer inner] is true when both boundaries of [inner] are within
    [outer]. *)
val contains : t -> t -> bool

(** [contains_position range position ~inclusive_end] tests whether [position]
    is within [range]. LSP ranges normally have an exclusive end; set
    [inclusive_end] when a cursor on the end boundary should count as contained. *)
val contains_position : t -> Position.t -> inclusive_end:bool -> bool

(** The non-empty intersection of two ranges. Touching ranges have no
    intersection. *)
val intersection : t -> t -> t option

(** Whether two ranges overlap. With [touching] set, ranges that share only a
    boundary are also considered to overlap. *)
val overlaps : t -> t -> touching:bool -> bool

val first_line : t
val to_string : t -> string
