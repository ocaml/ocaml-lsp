module Named = struct
  type 'a t =
    { name : string
    ; data : 'a
    }
end

module Type = struct
  module Prim = struct
    type t =
      | Number
      | Boolean
  end

  type t =
    | Name of string
    | Sum of t list
    | List of t
    | Prim of Prim.t
    | App of
        { name : string
        ; param : t
        }
end

module Field = struct
  type typ =
    | Const of string
    | Type of typ

  module Single = struct
    type t' =
      { optional : bool
      ; typ : typ
      }

    type t = t' Named.t
  end

  module Pattern = struct
    type t =
      { key : typ
      ; val_ : typ
      }
  end

  type t' =
    | Single of Single.t
    | Pattern of Pattern.t

  type t = t' Named.t
end

module Interface = struct
  type t =
    { extends : string list
    ; fields : Field.t list
    ; params : string list
    }
end

module Alias = struct
  type t = Type.t Named.t
end

module Enum = struct
  type ('a, 'b) constrs =
    | Int of 'a list
    | String of 'b list

  type t' =
    | Anon of (int, string) constrs
    | Named of (int Named.t, string Named.t) constrs

  type t = t' Named.t
end

type t =
  | Interface of Interface.t
  | Alias of Alias.t
  | Enum of Enum.t

let of_snippets _ = []
