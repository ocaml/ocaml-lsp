open Import

module Kind : sig
  type t =
    | Intf
    | Impl

  type ('intf, 'impl) pair =
    { intf : 'intf
    ; impl : 'impl
    }

  module Map : sig
    type 'a t = ('a, 'a) pair

    type kind

    val get : 'a t -> kind -> 'a

    val iter : 'a t -> f:('a -> unit) -> unit

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val both : 'a t -> 'b t -> ('a * 'b) t

    val make_both : 'a -> 'a t
  end
  with type kind := t
end

module Type : sig
  [@@@warning "-30"]

  type field

  type constr

  type prim =
    | Unit
    | String
    | Int
    | Bool

  type t =
    | Named of string
    | Var of string
    | Prim of prim
    | Tuple of t list
    | Optional of t
    | List of t
    | Poly_variant of constr list
    | Assoc of t * t
    | App of t * t list

  type decl =
    | Alias of t
    | Record of field list
    | Variant of constr list

  (* This is for lists where the keys are equal to strings *)
  val assoc_list : key:t -> data:t -> t

  val pp_decl : name:string -> kind:Kind.t -> decl -> unit Pp.t

  val pp : t -> kind:Kind.t -> unit Pp.t

  val field : t -> name:string -> field

  val constr : t list -> name:string -> constr

  (** Simplified sum types*)
  val enum : string list -> decl

  (** Polymorphic variant form *)
  val poly_enum : string list -> t

  val list : t -> t

  val module_t : string -> t

  val t : t

  val string : t

  val name : string -> t

  val int : t

  val bool : t

  val alpha : t

  val json : t

  val unit : t
end

module Expr : sig
  [@@@warning "-30"]

  type expr =
    | Ident of string
    | Let of pat * expr * expr
    | Match of expr * (pat * expr) list
    | Fun of pat arg list * expr
    | App of expr * expr arg list
    | Create of expr prim
    | Assert_false

  and 'e prim =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Ident of string
    | Cons of 'e * 'e prim
    | List of 'e list
    | Tuple of 'e list
    | Record of 'e record_
    | Constr of 'e constr

  and 'e arg =
    | Unnamed of 'e
    | Labeled of string * 'e
    | Optional of string * 'e

  and pat =
    | Wildcard
    | Pat of pat prim

  and 'e record_ = (string * 'e) list

  and 'e constr =
    { tag : string
    ; poly : bool
    ; args : 'e list
    }

  type t = expr

  val assert_false_clause : pat * expr

  type toplevel =
    { pat : (string arg * Type.t) list
    ; type_ : Type.t
    ; body : t
    }
end

module Module : sig
  type 'a t =
    { name : string
    ; bindings : 'a Named.t list
    }

  val empty : string -> 'a t

  type sig_ =
    | Value of Type.t
    | Type_decl of Type.decl
    | Json_conv_sig

  type impl =
    | Type_decl of Type.decl
    | Value of Expr.toplevel

  val pp_sig : sig_ t -> unit Pp.t

  val pp_impl : impl t -> unit Pp.t
end
