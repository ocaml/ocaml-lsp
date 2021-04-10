(** Helpers to generate OCaml code. Consider merging with ML *)

open Import

type t = unit Pp.t

type w = t

val surround : [ `Curly | `Paren | `Square ] -> 'a Pp.t -> 'a Pp.t

module Json : sig
  val invalid_pat : string -> w * w

  val typ : string

  module Literal : sig
    val str : string -> string

    val int : int -> string

    val null : string

    val bool : bool -> string
  end

  val str : string -> string

  val int : string -> string

  val bool : string -> string
end

module Attr : sig
  type t

  val make : string -> unit Pp.t list -> t
end

module Type : sig
  val string : w

  val int : w

  val bool : w

  val name : string -> w

  val and_ : string -> w -> w

  val decl : string -> w -> w

  val record : (string * w) list -> w

  val field_attrs : field:w -> attrs:Attr.t list -> w

  val rec_decls : (string * w) list -> w

  val var : string -> w

  val poly : (string * w list) list -> w

  val app : w -> w list -> w

  val tuple : w list -> w

  val deriving : w -> record:bool -> w

  val opt_attr : w

  val opt_field : w -> w

  val default : w -> string -> w

  val key : string -> w

  val variant : (string * w list) list -> w
end

module Sig : sig
  val module_ : string -> w -> w

  val include_ : string -> (w * w) list -> w

  val val_ : string -> w list -> w

  val assoc : w -> w -> w
end

val warnings : string -> w

val module_ : string -> w -> w

val opens : string list -> w

val record : (string * w) list -> w
