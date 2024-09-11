open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/polaritySearch"
let capability = "handlePolaritySearch", `Bool true

module PolaritySearchParams = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; position : Position.t
    ; query : string
    }

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let textDocumentPosition = Lsp.Types.TextDocumentPositionParams.t_of_yojson json in
    let query = json |> member "query" |> to_string in
    { position = textDocumentPosition.position
    ; text_document = textDocumentPosition.textDocument
    ; query
    }
  ;;

  let yojson_of_t { text_document; position; query } =
    `Assoc
      (("textDocument", TextDocumentIdentifier.yojson_of_t text_document)
       :: ("position", Position.yojson_of_t position)
       :: [ "query", `String query ])
  ;;
end

module PolaritySearch = struct
  type entry =
    { path : string
    ; desc : string
    }

  type t = entry list

  let entry_of_yojson json =
    let open Yojson.Safe.Util in
    let path = json |> member "path" |> to_string in
    let desc = json |> member "type" |> to_string in
    { path; desc }
  ;;

  let yojson_of_entry { path; desc } =
    `Assoc [ "path", `String path; "type", `String desc ]
  ;;

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    json |> to_list |> List.map ~f:entry_of_yojson
  ;;

  let yojson_of_t entries = `List (List.map ~f:yojson_of_entry entries)
end

type t = PolaritySearch.t

let t_of_yojson json = PolaritySearch.t_of_yojson json

module Request_params = struct
  type t = PolaritySearchParams.t

  let yojson_of_t t = PolaritySearchParams.yojson_of_t t
  let create text_document position query : t = { text_document; position; query }
end

let dispatch merlin position query =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let position = Position.logical position in
    let query = Query_protocol.Polarity_search (query, position) in
    let completions = Query_commands.dispatch pipeline query in
    PolaritySearch.yojson_of_t
      (List.map
         ~f:(fun entry ->
           { PolaritySearch.path = entry.Query_protocol.Compl.name; desc = entry.desc })
         completions.entries))
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let PolaritySearchParams.{ text_document; position; query } =
      PolaritySearchParams.t_of_yojson params
    in
    let uri = text_document.uri in
    let doc = Document_store.get state.State.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin -> dispatch merlin position query)
;;
