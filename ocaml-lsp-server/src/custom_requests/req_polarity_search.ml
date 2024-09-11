open Import
module TextDocumentPositionParams = Lsp.Types.TextDocumentPositionParams

let meth = "ocamllsp/polaritySearch"
let capability = "handlePolaritySearch", `Bool true

let completion_kind kind : CompletionItemKind.t =
  match kind with
  | `Value -> Value
  | `Variant -> EnumMember
  | `Label -> Field
  | `Module -> Module
  | `Modtype -> Interface
  | `MethodCall -> Method
  | `Keyword -> Keyword
  | `Constructor -> Constructor
  | `Type -> TypeParameter
;;

let kind_of_completion_kind (kind : CompletionItemKind.t) =
  match kind with
  | Value -> `Value
  | EnumMember -> `Variant
  | Field -> `Label
  | Module -> `Module
  | Interface -> `Modtype
  | Method -> `MethodCall
  | Keyword -> `Keyword
  | Constructor -> `Constructor
  | TypeParameter -> `Type
  | _ -> failwith "Unknown kind"
;;

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
  type t = Query_protocol.Compl.entry list

  let entry_of_yojson json =
    let open Yojson.Safe.Util in
    let name = json |> member "name" |> to_string in
    let kind =
      json |> member "kind" |> CompletionItemKind.t_of_yojson |> kind_of_completion_kind
    in
    let desc = json |> member "desc" |> to_string in
    let info = json |> member "info" |> to_string in
    let deprecated = json |> member "deprecated" |> to_bool in
    { Query_protocol.Compl.name; kind; desc; info; deprecated }
  ;;

  let yojson_of_entry { Query_protocol.Compl.name; kind; desc; info; deprecated } =
    `Assoc
      [ "name", `String name
      ; "kind", CompletionItemKind.yojson_of_t (completion_kind kind)
      ; "desc", `String desc
      ; "info", `String info
      ; "deprecated", `Bool deprecated
      ]
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
    match completions.context with
    | `Unknown -> PolaritySearch.yojson_of_t completions.entries
    | _ -> failwith "Wrong context")
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
