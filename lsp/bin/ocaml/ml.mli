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
    | App of t * t list

  type decl =
    | Alias of t
    | Record of field list
    | Variant of constr list

  val assoc_list : key:t -> data:t -> t

  val pp_decl : name:string -> kind:Kind.t -> decl -> unit Pp.t

  val pp : t -> unit Pp.t

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

module Module : sig
  type 'a t =
    { name : string
    ; bindings : 'a Named.t list
    }

  val empty : string -> 'a t

  type sig_ =
    | Value of Type.t
    | Type_decl of Type.decl

  type impl = Type_decl of Type.decl

  val pp_sig : sig_ t -> unit Pp.t

  val pp_impl : impl t -> unit Pp.t
end
