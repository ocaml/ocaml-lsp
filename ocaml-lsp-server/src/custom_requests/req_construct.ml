open Import

let capability = "handleConstruct", `Bool true
let meth = "ocamllsp/construct"

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; depth : int option
    ; with_values : [ `None | `Local ] option
    }

  let create ?with_values ?depth ~text_document ~position () =
    { text_document; position; with_values; depth }
  ;;

  let yojson_of_with_values = function
    | Some `Local -> `String "local"
    | Some `None -> `String "none"
    | None -> `Null
  ;;

  let yojson_of_t { text_document; position; with_values; depth } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let depth =
        ( "depth"
        , match depth with
          | None -> `Null
          | Some x -> `Int x )
      in
      let with_values = "withValues", yojson_of_with_values with_values in
      let position = "position", Position.yojson_of_t position in
      `Assoc (depth :: with_values :: position :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let with_values_of_yojson json =
    let open Yojson.Safe.Util in
    json
    |> member "withValues"
    |> to_string_option
    |> Option.bind ~f:(fun value ->
      match String.(lowercase_ascii @@ trim value) with
      | "none" -> Some `None
      | "local" -> Some `Local
      | _ -> None)
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = json |> TextDocumentIdentifier.t_of_yojson in
    let position = json |> member "position" |> Position.t_of_yojson in
    let depth = json |> member "depth" |> to_int_option in
    let with_values = json |> with_values_of_yojson in
    create ?with_values ?depth ~text_document ~position ()
  ;;
end

type t =
  { position : Range.t
  ; result : string list
  }

let t_of_yojson json =
  let open Yojson.Safe.Util in
  let position = json |> member "position" |> Range.t_of_yojson in
  let result = json |> member "result" |> to_list |> List.map ~f:to_string in
  { position; result }
;;

let yojson_of_t { position; result } =
  `Assoc
    [ "position", Range.yojson_of_t position
    ; "result", `List (List.map ~f:(fun x -> `String x) result)
    ]
;;

let with_pipeline state uri f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return `Null
  | `Merlin merlin ->
    (match Document.Merlin.kind merlin with
     | Document.Kind.Intf ->
       (* Construct makes no sense if its called from an interface. *)
       Fiber.return `Null
     | Document.Kind.Impl -> Document.Merlin.with_pipeline_exn merlin f)
;;

let make_construct_command position with_values depth =
  Query_protocol.Construct (position, with_values, depth)
;;

let dispatch_construct position with_values depth pipeline =
  let position = Position.logical position in
  let command = make_construct_command position with_values depth in
  let pos, result = Query_commands.dispatch pipeline command in
  yojson_of_t { position = Range.of_loc pos; result }
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Json.t) in
    let Request_params.{ text_document; position; with_values; depth } =
      Request_params.t_of_yojson params
    in
    let uri = text_document.uri in
    with_pipeline state uri @@ dispatch_construct position with_values depth)
;;
