open! Import

type t

include Json.Jsonable.S with type t := t

val compare : t -> t -> int

val equal : t -> t -> bool

val hash : t -> int

val to_path : t -> string

val of_path : string -> t

val to_string : t -> string

module Private : sig
  val win32 : bool ref
end
