open Import
open Fiber.O

let outline_kind kind : SymbolKind.t =
  match kind with
  | `Value -> Function
  | `Constructor -> Constructor
  | `Label -> Property
  | `Module -> Module
  | `Modtype -> Module
  | `Type -> String
  | `Exn -> Constructor
  | `Class -> Class
  | `Method -> Method

let rec symbol (item : Query_protocol.item) =
  let children = List.map item.children ~f:symbol in
  let range = Range.of_loc item.location in
  let kind = outline_kind item.outline_kind in
  DocumentSymbol.create
    ~name:item.outline_name
    ~kind
    ?detail:item.outline_type
    ~deprecated:item.deprecated
    ~range
    ~selectionRange:range
    ~children
    ()

let rec symbol_info ?containerName uri (item : Query_protocol.item) =
  let info =
    let kind = outline_kind item.outline_kind in
    let location = { Location.uri; range = Range.of_loc item.location } in
    SymbolInformation.create
      ~name:item.outline_name
      ~kind
      ~deprecated:false
      ~location
      ?containerName
      ()
  in
  let children =
    List.concat_map item.children ~f:(symbol_info uri ~containerName:info.name)
  in
  info :: children

let symbols_of_outline uri outline =
  List.concat_map ~f:(symbol_info uri) outline

let run (client_capabilities : ClientCapabilities.t) doc uri =
  let+ outline = Document.dispatch_exn doc Outline in
  match
    Option.value
      ~default:false
      (let open Option.O in
      let* textDocument = client_capabilities.textDocument in
      let* ds = textDocument.documentSymbol in
      ds.hierarchicalDocumentSymbolSupport)
  with
  | true ->
    let symbols = List.map outline ~f:symbol in
    `DocumentSymbol symbols
  | false -> `SymbolInformation (symbols_of_outline uri outline)
