module Option = struct
  let map t ~f =
    match t with
    | None -> None
    | Some x -> Some (f x)
end

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
