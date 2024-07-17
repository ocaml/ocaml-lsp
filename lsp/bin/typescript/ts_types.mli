open Import

module Literal : sig
  type t =
    | String of string
    | Int of int
    | Float of float

  val to_maybe_quoted_string : t -> string
  val to_dyn : t -> Dyn.t
end

module Enum : sig
  type case =
    | Literal of Literal.t
    | Alias of string

  val dyn_of_case : case -> Dyn.t

  type t = (string * case) list

  val to_dyn : (string * case) list -> Dyn.t
end

module type S = sig
  type ident

  type field_def =
    | Single of
        { optional : bool
        ; typ : typ
        }
    | Pattern of
        { pat : typ
        ; typ : typ
        }

  and field = field_def Named.t

  and typ =
    | Literal of Literal.t
    | Ident of ident
    | Sum of typ list
    | List of typ
    | Record of field list
    | Tuple of typ list
    | App of typ * typ

  and interface = { fields : field list }

  and decl =
    | Interface of interface
    | Type of typ
    | Enum_anon of Enum.t

  and t = decl Named.t

  val to_dyn : t -> Dyn.t
  val dyn_of_typ : typ -> Dyn.t
  val dyn_of_field : field -> Dyn.t

  class map : object
    method enum_anon : Enum.t -> Enum.t
    method field : field -> field
    method interface : interface -> interface
    method sum : typ list -> typ
    method t : t -> t
    method typ : typ -> typ
  end

  class ['a] fold : object
    method field : field -> init:'a -> 'a
    method ident : ident -> init:'a -> 'a
    method t : t -> init:'a -> 'a
    method typ : typ -> init:'a -> 'a
  end
end

module Unresolved : sig
  include S with type ident := String.t

  val enum : name:string -> constrs:Enum.t -> Enum.t Named.t
  val interface : name:string -> fields:field list -> interface Named.t
  val pattern_field : name:string -> pat:typ -> typ:typ -> field_def Named.t
  val named_field : ?optional:bool -> typ -> string -> field_def Named.t
end

module Ident : sig
  module Id : Id.S

  type t =
    { id : Id.t
    ; name : string
    }

  val to_dyn : t -> Dyn.t
  val make : string -> t

  module Top_closure : sig
    val top_closure
      :  key:('a -> t)
      -> deps:('a -> 'a list)
      -> 'a list
      -> ('a list, 'a list) result
  end
end

module Prim : sig
  type t =
    | Null
    | String
    | Bool
    | Number
    | Uinteger
    | Uri
    | Any
    | Object
    | List
    | Self
    | Resolved of Ident.t

  val to_dyn : t -> Dyn.t
  val of_string : string -> resolve:(string -> t) -> t
end

module Resolved : S with type ident := Prim.t

val resolve_all : Unresolved.t list -> names:Ident.t String.Map.t -> Resolved.t list
