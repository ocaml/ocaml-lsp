type t

val of_slice : string -> pos:int -> len:int -> t
val of_string : string -> t
val compare : t -> t -> int
val concat : t Array_view.t -> string
val take : t -> int -> t
val drop : t -> int -> t
val to_string : t -> string
val length : t -> int
val add_buffer : t -> Buffer.t -> unit
val split_at : t -> int -> t * t
val rsplit_at : t -> int -> t * t
val index_from : t -> pos:int -> char -> int option
val rindex : t -> char -> int option
val rindex_from : t -> pos:int -> char -> int option
val get_exn : t -> int -> char

type move =
  { newlines : int
  ; consumed : int
  }

val move_left : t -> pos:int -> len:int -> move
val move_right : t -> pos:int -> len:int -> move
val blit : t -> dst:bytes -> dst_pos:int -> unit

module Uutf : sig
  val src : t -> pos:int -> Uutf.decoder -> unit
end

module Map : MoreLabels.Map.S with type key = t
