open Import

let meth = "ocamllsp/locate"
let capability = "handleLocate", `Bool true

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; kind : [ `Type_definition | `Definition | `Declaration ]
    ; position : Position.t
    ; prefix : string option
    }

  let create ?prefix ~text_document ~kind ~position () =
    { text_document; position; prefix; kind }
  ;;

  let yojson_of_t { text_document; kind; position; prefix } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let position = "position", Position.yojson_of_t position
      and kind =
        ( "kind"
        , `String
            (match kind with
             | `Type_definition -> "type-definition"
             | `Declaration -> "declaration"
             | `Definition -> "definition") )
      and prefix =
        ( "prefix"
        , match prefix with
          | None -> `Null
          | Some p -> `String p )
      in
      `Assoc (position :: prefix :: kind :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let kind_of_yojson json =
    let open Yojson.Safe.Util in
    json
    |> member "kind"
    |> to_string_option
    |> Option.map ~f:String.lowercase_ascii
    |> function
    | Some "type-definition" -> `Type_definition
    | Some "declaration" -> `Declaration
    | _ -> `Definition
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = json |> TextDocumentIdentifier.t_of_yojson
    and kind = kind_of_yojson json
    and position = json |> member "position" |> Position.t_of_yojson
    and prefix = json |> member "prefix" |> to_string_option in
    create ~text_document ~position ~kind ?prefix ()
  ;;
end

type t = Location.t

let t_of_yojson = Location.t_of_yojson

let yojson_of_t = function
  | Some (`Location locs) -> `List (List.map ~f:Location.yojson_of_t locs)
  | _ -> `Null
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let open Fiber.O in
    let Request_params.{ text_document = { uri }; position; prefix; kind } =
      (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t)
      |> Request_params.t_of_yojson
    in
    let+ result = Definition_query.run ?prefix kind state uri position in
    yojson_of_t result)
;;
