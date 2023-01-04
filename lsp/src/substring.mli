type t

val of_slice : string -> pos:int -> len:int -> t

val compare : t -> t -> int

val concat : t Array_view.t -> string

module Map : MoreLabels.Map.S with type key = t
