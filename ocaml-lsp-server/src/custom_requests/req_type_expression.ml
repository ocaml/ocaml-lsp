open Import

let meth = "ocamllsp/typeExpression"
let capability = "handleTypeExpression", `Bool true

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; expression : string
    }

  let create ~text_document ~position ~expression =
    { text_document; position; expression }
  ;;

  let yojson_of_t { text_document; position; expression } =
    match TextDocumentIdentifier.yojson_of_t text_document with
    | `Assoc assoc ->
      let position = "position", Position.yojson_of_t position
      and expression = "expression", `String expression in
      `Assoc (position :: expression :: assoc)
    | _ -> (* unreachable *) assert false
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document = json |> TextDocumentIdentifier.t_of_yojson
    and position = json |> member "position" |> Position.t_of_yojson
    and expression = json |> member "expression" |> to_string in
    create ~text_document ~position ~expression
  ;;
end

type t = string

let t_of_yojson = Yojson.Safe.Util.to_string

let with_pipeline state uri f =
  let doc = Document_store.get state.State.store uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin -> Document.Merlin.with_pipeline_exn merlin f
;;

let make_type_expr_command position expression =
  Query_protocol.Type_expr (expression, position)
;;

let dispatch_type_expr position expression pipeline =
  let position = Position.logical position in
  let command = make_type_expr_command position expression in
  let result = Query_commands.dispatch pipeline command in
  Some result
;;

let on_request ~params state =
  let open Fiber.O in
  Fiber.of_thunk (fun () ->
    let Request_params.{ text_document = { uri }; position; expression } =
      (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t)
      |> Request_params.t_of_yojson
    in
    let* typ = with_pipeline state uri (dispatch_type_expr position expression) in
    match typ with
    | Some typ ->
      let* result = Ocamlformat_rpc.format_type ~typ state.ocamlformat_rpc in
      (match result with
       | Error _ -> Fiber.return (`String typ)
       | Ok typ -> Fiber.return (`String (String.trim ~drop:(Char.equal '\n') typ)))
    | None -> Fiber.return `Null)
;;
