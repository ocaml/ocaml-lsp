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
    }

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let textDocumentPosition = Lsp.Types.TextDocumentPositionParams.t_of_yojson json in
    let query = json |> member "query" |> to_string in
    let limit = json |> member "limit" |> to_int in
    let with_doc = json |> member "with_doc" |> to_bool in
    { position = textDocumentPosition.position
    ; text_document = textDocumentPosition.textDocument
    ; query
    ; limit
    ; with_doc
    }
  ;;

  let yojson_of_t { text_document; position; query; limit; with_doc } =
    `Assoc
      (("textDocument", TextDocumentIdentifier.yojson_of_t text_document)
       :: ("position", Position.yojson_of_t position)
       :: ("limit", `Int limit)
       :: ("with_doc", `Bool with_doc)
       :: [ "query", `String query ])
  ;;
end

module TypeSearch = struct
  type t = Query_protocol.type_search_result list

  let yojson_of_t (t : t) =
    let yojson_of_type_search_result (res : Query_protocol.type_search_result) =
      `Assoc
        [ "name", `String res.name
        ; "typ", `String res.typ
        ; "loc", Range.yojson_of_t (Range.of_loc res.loc)
        ; ( "doc"
          , match res.doc with
            | Some d -> `String d
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

  let create text_document position limit query with_doc : t =
    { text_document; position; limit; query; with_doc }
  ;;
end

let dispatch merlin position limit query with_doc =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let position = Position.logical position in
    let query = Query_protocol.Type_search (query, position, limit, with_doc) in
    let results = Query_commands.dispatch pipeline query in
    TypeSearch.yojson_of_t results)
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let TypeSearchParams.{ text_document; position; limit; query; with_doc } =
      TypeSearchParams.t_of_yojson params
    in
    let uri = text_document.uri in
    let doc = Document_store.get state.State.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin -> dispatch merlin position limit query with_doc)
;;
