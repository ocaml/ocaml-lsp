module Named = struct
  type 'a t =
    { name : string
    ; data : 'a
    }

  let make ~name data = { name; data }
end

module Literal = struct
  type t =
    | String of string
    | Int of int
    | Float of float
end

module Enum = struct
  type t = (string * Literal.t) list
end

module type S = sig
  type ident

  type field_def =
    | Single of { optional : bool ; typ: typ }
    | Pattern of  { pat : typ; typ: typ }

  and field = field_def Named.t

  and typ =
    | Literal of Literal.t
    | Ident of ident
    | Sum of typ list
    | List of typ
    | Record of field list
    | Tuple of typ list
    | App of typ * typ

  and interface =
    { extends : ident list
    ; fields : field list
    ; params : string list
    }

  and decl =
    | Interface of interface
    | Alias of typ
    | Type of typ
    | Enum_anon of Enum.t

  and t = decl Named.t
end

module Make (Ident : sig type t end) = struct
  type field_def =
    | Single of { optional : bool ; typ: typ }
    | Pattern of  { pat : typ; typ: typ }

  and field = field_def Named.t

  and typ =
    | Literal of Literal.t
    | Ident of Ident.t
    | Sum of typ list
    | List of typ
    | Record of field list
    | Tuple of typ list
    | App of typ * typ

  and interface =
    { extends : Ident.t list
    ; fields : field list
    ; params : string list
    }

  and decl =
    | Interface of interface
    | Alias of typ
    | Type of typ
    | Enum_anon of Enum.t

  and t = decl Named.t
end

module Unresolved = struct
  include Make(String)

  let enum ~name ~constrs : Enum.t Named.t = { Named.name; data = constrs }

  let interface ~name ~extends ~fields ~params : interface Named.t =
    { Named.name; data = { extends; fields; params } }

  let pattern_field ~name ~pat ~typ =
    { Named.name; data = Pattern { pat; typ } }

  let named_field ?(optional = false) typ name =
    { Named.name; data = Single { optional ; typ } }
end

module type Prim_intf = sig
  type resolved

  type t =
    | Null
    | String
    | Bool
    | Number
    | Any
    | Object
    | Resolved of resolved

  val of_string : string -> resolve:(string -> t) -> t
end

module Prim_make (Resolved : sig type t end) = struct

  type t =
    | Null
    | String
    | Bool
    | Number
    | Any
    | Object
    | Resolved of Resolved.t

  let of_string s ~resolve =
    match String.lowercase_ascii s with
    | "null" -> Null
    | "string" -> String
    | "boolean" -> Bool
    | "number" -> Number
    | "any" -> Any
    | "object" -> Object
    | _ -> resolve s
end

module rec Resolved : S with type ident := Prim.t = Make(Prim)
and Prim : Prim_intf with type resolved := Resolved.t = Prim_make(Resolved)
