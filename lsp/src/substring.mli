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

type termination =
  { newline : bool
  ; consumed : int
  }

val count_upto_chars_or_newline_backwards : t -> int -> termination

val count_upto_chars_or_newline : t -> int -> termination

val blit : t -> dst:bytes -> dst_pos:int -> unit

module Map : MoreLabels.Map.S with type key = t
