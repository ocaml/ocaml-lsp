module Named = struct
  type 'a t =
    { name : string
    ; data : 'a
    }

  let make ~name data = { name; data }
end

module Field = struct

  type 't t' =
    | Single of { optional : bool ; typ: 't }
    | Pattern of  { pat : 't; typ: 't }

  let pattern ~name ~pat ~typ =
    { Named.name; data = Pattern { pat; typ } }

  let named ?(optional = false) typ name =
    { Named.name; data = Single { optional ; typ } }

  type nonrec 't t = 't t' Named.t
end

module Literal = struct
  type t =
    | String of string
    | Int of int
    | Float of float
end

module Type = struct
  type t =
    | Literal of Literal.t
    | Name of string
    | Sum of t list
    | List of t
    | Record of t Field.t list
    | Tuple of t list
    | App of t * t
end

module Interface = struct
  type t =
    { extends : string list
    ; fields : Type.t Field.t list
    ; params : string list
    }

  let make ~name ~extends ~fields ~params =
    { Named.name; data = { extends; fields; params } }
end

module Alias = struct
  type t = Type.t Named.t
end

module Enum = struct
  type t = (string * Literal.t) list Named.t

  let named ~name ~constrs = { Named.name; data = constrs }
end

type t =
  | Interface of Interface.t Named.t
  | Alias of Alias.t
  | Type of Type.t Named.t
  | Enum_anon of Enum.t
