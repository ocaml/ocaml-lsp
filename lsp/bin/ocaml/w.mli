(** Generation ocaml code *)

open Import

module Json : sig
  val invalid_pat : string -> 'a Pp.t * 'b Pp.t

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

module Type : sig
  val and_ : string -> 'a Pp.t -> 'a Pp.t

  val decl : string -> 'a Pp.t -> 'a Pp.t

  val record : (string * 'a Pp.t) list -> 'a Pp.t

  val rec_decls : (string * 'a Pp.t) list -> 'a Pp.t

  val deriving : 'a Pp.t -> 'a Pp.t

  val opt_attr : 'a Pp.t

  val opt_field : 'a Pp.t -> 'a Pp.t

  val default : 'a Pp.t -> string -> 'a Pp.t

  val key : string -> 'a Pp.t

  val variant_ : (string * 'a Pp.t option) list -> 'a Pp.t
end

module Sig : sig
  val module_ : string -> 'a Pp.t -> 'a Pp.t

  val val_ : string -> 'a Pp.t list -> 'a Pp.t

  val tuple : 'a Pp.t list -> 'a Pp.t

  val assoc : 'a Pp.t -> 'a Pp.t -> 'a Pp.t

  module Json : sig
    val arr : string -> 'a Pp.t list

    val to_json : string -> 'a Pp.t

    val of_json : string -> 'a Pp.t
  end
end

val warnings : string -> 'a Pp.t

val match_ : string -> ('a Pp.t * 'a Pp.t) list -> 'a Pp.t

val module_ : string -> 'a Pp.t -> 'a Pp.t

val opens : string list -> 'a Pp.t

val of_json : string -> 'a Pp.t -> 'a Pp.t

val to_json : string -> 'a Pp.t -> 'a Pp.t

val record : (string * 'a Pp.t) list -> 'a Pp.t
