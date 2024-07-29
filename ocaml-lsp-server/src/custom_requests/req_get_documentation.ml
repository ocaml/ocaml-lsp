open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/getDocumentation"
let capability = "handleGetDocumentation", `Bool true

module GetDocClientCapabilities = struct
  type _t = { contentFormat : MarkupKind.t list }

  let _yojson_of_t { contentFormat } =
    `Assoc
      (`List (List.map ~f:(fun format -> MarkupKind.yojson_of_t format) contentFormat))
  ;;
end

module GetDocParams = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; identifier : string option
    ; contentFormat : MarkupKind.t option
    }

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let textDocumentPosition = Lsp.Types.TextDocumentPositionParams.t_of_yojson json in
    let identifier = json |> member "identifier" |> to_option to_string in
    let contentFormat =
      json |> member "contentFormat" |> to_option MarkupKind.t_of_yojson
    in
    { position = textDocumentPosition.position
    ; text_document = textDocumentPosition.textDocument
    ; identifier
    ; contentFormat
    }
  ;;

  let yojson_of_t { text_document; position; identifier; contentFormat } =
    let identifier =
      match identifier with
      | Some ident -> [ "identifier", `String ident ]
      | None -> []
    in
    let contentFormat =
      match contentFormat with
      | Some format -> [ "contentFormat", MarkupKind.yojson_of_t format ]
      | None -> []
    in
    `Assoc
      ((("textDocument", TextDocumentIdentifier.yojson_of_t text_document)
        :: ("position", Position.yojson_of_t position)
        :: identifier)
       @ contentFormat)
  ;;
end

module GetDoc = struct
  type t = { doc : MarkupContent.t }

  let yojson_of_t { doc } = `Assoc [ "doc", MarkupContent.yojson_of_t doc ]

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let doc = json |> member "doc" |> MarkupContent.t_of_yojson in
    { doc }
  ;;

  let create ~kind ~value =
    let v =
      match kind with
      | MarkupKind.Markdown ->
        (match Doc_to_md.translate value with
         | Raw d -> d
         | Markdown d -> d)
      | MarkupKind.PlainText -> value
    in
    MarkupContent.create ~kind ~value:v
  ;;
end

type t = GetDoc.t

let t_of_yojson json = GetDoc.t_of_yojson json

module Request_params = struct
  type t = GetDocParams.t

  let yojson_of_t t = GetDocParams.yojson_of_t t

  let create ~text_document ~position ?(identifier = None) ?(contentFormat = None) () : t =
    { text_document; identifier; contentFormat; position }
  ;;
end

let dispatch ~merlin ~position ~identifier ~contentFormat =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let position = Position.logical position in
    let query = Query_protocol.Document (identifier, position) in
    let result = Query_commands.dispatch pipeline query in
    match result with
    | `No_documentation | `Invalid_context | `Not_found _ -> `Null
    | `Builtin value | `File_not_found value | `Found value | `Not_in_env value ->
      let kind =
        match contentFormat with
        | Some format -> format
        | None -> MarkupKind.PlainText
      in
      GetDoc.yojson_of_t { doc = GetDoc.create ~kind ~value })
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let GetDocParams.{ text_document; position; identifier; contentFormat } =
      GetDocParams.t_of_yojson params
    in
    let uri = text_document.uri in
    let doc = Document_store.get state.State.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin -> dispatch ~merlin ~position ~identifier ~contentFormat)
;;
