(** Representation of OCaml code used for generation *)

open Import

val is_kw : string -> bool

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

module Arg : sig
  (** Represent arrow types and argument patterns *)

  type 'e t =
    | Unnamed of 'e
    | Labeled of string * 'e
    | Optional of string * 'e
end

module Path : sig
  type t =
    | Ident of string
    | Dot of t * string
    | Apply of t * t

  val to_string : t -> string
end

module Type : sig
  [@@@warning "-30"]

  type prim =
    | Unit
    | String
    | Int
    | Bool

  type t =
    | Path of Path.t
    | Var of string
    | Prim of prim
    | Tuple of t list
    | Optional of t
    | List of t
    | Poly_variant of constr list
    | Assoc of t * t
    | App of t * t list
    | Fun of t Arg.t * t

  and field =
    { name : string
    ; typ : t
    ; attrs : (string * string list) list
    }

  and constr =
    { name : string
    ; args : t list
    }

  type decl =
    | Alias of t
    | Record of field list
    | Variant of constr list

  val fun_ : t Arg.t list -> t -> t

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

  val void : t

  (** Fold and map over a type expression.

      ['m] is the type of monoid summarized.

      ['env] is a custom value threaded through the path. Parent nodes can use
      this to give child nodes context *)
  class virtual ['env, 'm] mapreduce :
    object ('self)
      method virtual empty : 'm

      method virtual plus : 'm -> 'm -> 'm

      (** doesn't really to be here, but putting it here avoids passing [empty]
          and [plus] to a general purpose [fold_left_map]*)
      method private fold_left_map :
        'a. f:('a -> 'a * 'm) -> 'a list -> 'a list * 'm

      method alias : 'env -> t -> decl * 'm

      method app : 'env -> t -> t list -> t * 'm

      method assoc : 'env -> t -> t -> t * 'm

      method constr : 'env -> constr -> constr * 'm

      method field : 'env -> field -> field * 'm

      method list : 'env -> t -> t * 'm

      method path : 'env -> Path.t -> t * 'm

      method optional : 'env -> t -> t * 'm

      method poly_variant : 'env -> constr list -> t * 'm

      method prim : 'env -> prim -> t * 'm

      method record : 'env -> field list -> decl * 'm

      method t : 'env -> t -> t * 'm

      method decl : 'env -> decl -> decl * 'm

      method tuple : 'env -> t list -> t * 'm

      method var : 'env -> string -> t * 'm

      method variant : 'env -> constr list -> decl * 'm
    end
end

module Expr : sig
  (** An (untyped) ocaml expression. It is the responsibility of the generator
      to create well typed expressions *)
  type expr =
    | Let of pat * expr * expr
    | Match of expr * (pat * expr) list
    | Fun of pat Arg.t list * expr
    | App of expr * expr Arg.t list
    | Create of expr prim
    | Assert_false

  (* patterns or constructors, depending on ['e] *)
  and 'e prim =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    (* This should be Path.t as well *)
    | Ident of string
    | Cons of 'e * 'e prim
    | List of 'e list
    | Tuple of 'e list
    | Record of 'e record_
    | Constr of 'e constr

  and pat =
    | Wildcard  (** [_ -> ] *)
    | Pat of pat prim

  and 'e record_ = (string * 'e) list

  and 'e constr =
    { tag : string  (** the tag in a tagged union *)
    ; poly : bool  (** polymorphic variant? *)
    ; args : 'e list
    }

  type t = expr

  (** [ _ -> assert false ] *)
  val assert_false_clause : pat * expr

  (** toplevel declartion (without the name) *)
  type toplevel =
    { pat : (string Arg.t * Type.t) list
          (** paterns and their types. types should be optional but they really
              help the error messages if the generated code is incorrect *)
    ; type_ : Type.t  (** useful to annotate the return types *)
    ; body : t
    }
end

module Module : sig
  (** Generate OCaml modules with JS converters *)
  module Name : sig
    type t = private string

    val of_string : string -> t
  end

  type 'a t =
    { name : Name.t
    ; bindings : 'a Named.t list
    }

  val empty : Name.t -> 'a t

  type sig_ =
    | Value of Type.t
    | Type_decl of Type.decl
    | Include of Name.t * (Type.t * Type.t) list

  type impl =
    | Type_decl of Type.decl
    | Value of Expr.toplevel

  val pp_sig : sig_ t -> unit Pp.t

  val pp_impl : impl t -> unit Pp.t
end
