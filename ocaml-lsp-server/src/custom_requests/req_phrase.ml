open Import

let meth = "ocamllsp/phrase"
let capability = "handlePhrase", `Bool true

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; target : [ `Prev | `Next ]
    }

  let create ~text_document ~position ~target = { text_document; position; target }

  let yojson_of_t { text_document; position; target } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let position = "position", Position.yojson_of_t position
      and target =
        ( "target"
        , `String
            (match target with
             | `Next -> "next"
             | `Prev -> "prev") )
      in
      `Assoc (position :: target :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let target_of_yojson json =
    let open Yojson.Safe.Util in
    json
    |> member "target"
    |> to_string_option
    |> Option.map ~f:String.lowercase_ascii
    |> function
    | Some "next" -> `Next
    | Some "prev" -> `Prev
    | _ -> `Next
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = json |> TextDocumentIdentifier.t_of_yojson
    and position = json |> member "position" |> Position.t_of_yojson
    and target = target_of_yojson json in
    create ~text_document ~position ~target
  ;;
end

type t = Position.t

let t_of_yojson x = Position.t_of_yojson x

let with_pipeline state uri f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return `Null
  | `Merlin merlin -> Document.Merlin.with_pipeline_exn merlin f
;;

let make_phrase_command position target = Query_protocol.Phrase (target, position)

let dispatch_phrase position target pipeline =
  let position = Position.logical position in
  let command = make_phrase_command position target in
  let result = Query_commands.dispatch pipeline command in
  match Position.of_lexical_position result with
  | None -> `Null
  | Some pos -> Position.yojson_of_t pos
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let Request_params.{ text_document = { uri }; position; target } =
      (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t)
      |> Request_params.t_of_yojson
    in
    with_pipeline state uri @@ dispatch_phrase position target)
;;
