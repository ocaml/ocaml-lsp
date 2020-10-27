module String = Stdune.String
module Array = Stdune.Array
module List = Stdune.List
module Option = Stdune.Option
module Dyn = Stdune.Dyn
module Either = Stdune.Either

module Json = struct
  type t = Yojson.Safe.t

  let to_pretty_string (t : t) = Yojson.Safe.pretty_to_string ~std:false t

  let to_string t = Yojson.Safe.to_string t

  let of_string s = Yojson.Safe.from_string s

  let yojson_of_t x = x

  let t_of_yojson x = x

  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

  let yojson_of_list = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_list

  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t

  module Jsonable = Ppx_yojson_conv_lib.Yojsonable

  let field fields name conv = List.assoc_opt name fields |> Option.map ~f:conv

  let field_exn fields name conv =
    match field fields name conv with
    | Some f -> f
    | None -> error "Jsonrpc.Result.t: missing field" (`Assoc fields)

  let rec of_dyn (t : Dyn.t) : t =
    match t with
    | Opaque -> `String "<opaque>"
    | Unit -> `String "()"
    | Int i -> `Int i
    | Int64 i -> `Int (Int64.to_int i)
    | Bool b -> `Bool b
    | String s -> `String s
    | Bytes s -> `String (Bytes.to_string s)
    | Char c -> `String (String.of_list [ c ])
    | Float f -> `Float f
    | Option None -> `String "<none>"
    | Option (Some s) -> of_dyn s
    | List xs -> `List (List.map ~f:of_dyn xs)
    | Array xs -> `List (List.map ~f:of_dyn (Array.to_list xs))
    | Tuple xs -> `List (List.map ~f:of_dyn xs)
    | Record r -> `Assoc (List.map r ~f:(fun (k, v) -> (k, of_dyn v)))
    | Variant (name, args) -> `Assoc [ (name, of_dyn (List args)) ]
    | Set xs -> `List (List.map ~f:of_dyn xs)
    | Map map ->
      `List (List.map map ~f:(fun (k, v) -> `List [ of_dyn k; of_dyn v ]))

  module Conv = struct
    include Ppx_yojson_conv_lib.Yojson_conv
  end

  module O = struct
    let ( <|> ) c1 c2 json =
      match c1 json with
      | s -> s
      | exception Conv.Of_yojson_error (_, _) -> c2 json
  end

  module Option = struct
    type 'a t = 'a option

    let yojson_of_t f = function
      | None -> `Null
      | Some x -> f x

    let t_of_yojson f = function
      | `Null -> None
      | json -> Some (f json)
  end
end
