open! Import

type t

val content_length : t -> int
val content_type : t -> string
val create : ?content_type:string -> content_length:int -> unit -> t
val to_string : t -> string

module Private : sig
  module Key : sig
    val content_length : string
    val content_type : string
  end
end
