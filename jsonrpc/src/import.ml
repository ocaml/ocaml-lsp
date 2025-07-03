module List = ListLabels

module Option = struct
  let map t ~f =
    match t with
    | None -> None
    | Some x -> Some (f x)
  ;;
end

module Json = struct
  type t = Yojson.Safe.t

  exception Of_json of (string * t)

  let () =
    Printexc.register_printer (function
      | Of_json (msg, _) -> Some ("Jsonrpc: json conversion failed: " ^ msg)
      | _ -> None)
  ;;

  let error msg json = raise (Of_json (msg, json))

  module Jsonable = struct
    module type S = sig
        type json
        type t

        val yojson_of_t : t -> json
        val t_of_yojson : json -> t
      end
      with type json := t
  end

  let field fields name conv = List.assoc_opt name fields |> Option.map ~f:conv

  let field_exn fields name conv =
    match field fields name conv with
    | Some f -> f
    | None -> error ("missing field " ^ name) (`Assoc fields)
  ;;

  module Conv = struct
    let string_of_yojson = function
      | `String s -> s
      | json -> error "expected string" json
    ;;
  end
end
