open Import

include Yojsonable.S

val to_path : t -> string

val of_path : string -> t

val to_string : t -> string

val pp : Format.formatter -> t -> unit
