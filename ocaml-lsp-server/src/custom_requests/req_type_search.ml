open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/typeSearch"
let capability = "handleTypeSearch", `Bool true

module TypeSearchParams = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; limit : int
    ; query : string
    ; with_doc : bool
    ; doc_format : MarkupKind.t option
    }

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let textDocumentPosition = Lsp.Types.TextDocumentPositionParams.t_of_yojson json in
    let query = json |> member "query" |> to_string in
    let limit = json |> member "limit" |> to_int in
    let with_doc = json |> member "with_doc" |> to_bool in
    let doc_format = json |> member "doc_format" |> to_option MarkupKind.t_of_yojson in
    { position = textDocumentPosition.position
    ; text_document = textDocumentPosition.textDocument
    ; query
    ; limit
    ; with_doc
    ; doc_format
    }
  ;;

  let yojson_of_t { text_document; position; query; limit; with_doc; doc_format } =
    let doc_format =
      match doc_format with
      | Some format -> [ "doc_format", MarkupKind.yojson_of_t format ]
      | None -> []
    in
    `Assoc
      (("textDocument", TextDocumentIdentifier.yojson_of_t text_document)
       :: ("position", Position.yojson_of_t position)
       :: ("limit", `Int limit)
       :: ("with_doc", `Bool with_doc)
       :: ("query", `String query)
       :: doc_format)
  ;;
end

module TypeSearch = struct
  type t = string Query_protocol.type_search_result list

  let doc_to_markupContent ~kind ~value =
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

  let yojson_of_t (t : t) doc_format =
    let format =
      match doc_format with
      | Some format -> format
      | None -> MarkupKind.PlainText
    in
    let yojson_of_type_search_result (res : string Query_protocol.type_search_result) =
      `Assoc
        [ "name", `String res.name
        ; "typ", `String res.typ
        ; "loc", Range.yojson_of_t (Range.of_loc res.loc)
        ; ( "doc"
          , match res.doc with
            | Some value ->
              doc_to_markupContent ~kind:format ~value |> MarkupContent.yojson_of_t
            | None -> `Null )
        ; "cost", `Int res.cost
        ; "constructible", `String res.constructible
        ]
    in
    `List (List.map ~f:yojson_of_type_search_result t)
  ;;
end

type t = TypeSearch.t

module Request_params = struct
  type t = TypeSearchParams.t

  let yojson_of_t t = TypeSearchParams.yojson_of_t t

  let create text_document position limit query with_doc doc_format : t =
    { text_document; position; limit; query; with_doc; doc_format }
  ;;
end

let dispatch merlin position limit query with_doc doc_format =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let position = Position.logical position in
    let query = Query_protocol.Type_search (query, position, limit, with_doc) in
    let results = Query_commands.dispatch pipeline query in
    TypeSearch.yojson_of_t results doc_format)
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let TypeSearchParams.{ text_document; position; limit; query; with_doc; doc_format } =
      TypeSearchParams.t_of_yojson params
    in
    let uri = text_document.uri in
    let doc = Document_store.get state.State.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin -> dispatch merlin position limit query with_doc doc_format)
;;
