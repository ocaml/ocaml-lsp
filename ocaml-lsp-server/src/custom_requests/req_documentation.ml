open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/documentation"

let capability = ("handleDocumentation", `Bool true)

module GetDocClientCapabilities = struct
  type t = { contentFormat : MarkupKind.t list }

  let yojson_of_t { contentFormat } =
    `Assoc
      (`List
        (List.map
           ~f:(fun format -> MarkupKind.yojson_of_t format)
           contentFormat))
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
    let textDocumentPosition =
      Lsp.Types.TextDocumentPositionParams.t_of_yojson json
    in
    let identifier = json |> member "identifier" |> to_option to_string in
    let contentFormat =
      json |> member "contentFormat" |> to_option MarkupKind.t_of_yojson
    in
    { position = textDocumentPosition.position
    ; text_document = textDocumentPosition.textDocument
    ; identifier
    ; contentFormat
    }
end

module GetDoc = struct
  type t = { doc : MarkupContent.t }

  let yojson_of_t { doc } = `Assoc [ ("doc", MarkupContent.yojson_of_t doc) ]

  let create ~kind ~value = MarkupContent.create ~kind ~value
end

type t = GetDoc.t

let dispatch ~merlin ~position ~identifier ~contentFormat =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
      let position = Position.logical position in
      let query = Query_protocol.Document (identifier, position) in
      let result = Query_commands.dispatch pipeline query in
      match result with
      | `No_documentation | `Invalid_context | `Not_found _ -> `Null
      | `Builtin value
      | `File_not_found value
      | `Found value
      | `Not_in_env value ->
        let markup_content =
          match contentFormat with
          | Some format -> GetDoc.create ~kind:format ~value
          | None -> GetDoc.create ~kind:MarkupKind.PlainText ~value
        in
        GetDoc.yojson_of_t { doc = markup_content })

let on_request ~params state =
  Fiber.of_thunk (fun () ->
      let params =
        (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t)
      in
      let GetDocParams.{ text_document; position; identifier; contentFormat } =
        GetDocParams.t_of_yojson params
      in
      let uri = text_document.uri in
      let doc = Document_store.get state.State.store uri in
      match Document.kind doc with
      | `Other -> Fiber.return `Null
      | `Merlin merlin -> dispatch ~merlin ~position ~identifier ~contentFormat)
