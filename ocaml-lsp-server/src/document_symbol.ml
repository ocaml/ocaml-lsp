open Import
open Fiber.O

let symbol_kind_of_outline_kind = function
  | `Value -> Lsp.Types.SymbolKind.Variable
  | `Constructor | `Exn -> EnumMember
  | `Label -> EnumMember
  | `Module -> Module
  | `Type -> TypeParameter
  | `Class -> Class
  | `ClassType | `Modtype -> Interface
  | `Method -> Method
;;

let rec items_to_symbols items =
  List.rev_map
    ~f:
      (fun
        { Query_protocol.outline_name; outline_kind; location; selection; children; _ } ->
      let range = Range.of_loc location in
      (* The LSP spec requires [selectionRange] to be contained in [range].
         Merlin can return a [selection] that violates this invariant (e.g. for
         the synthetic binding of an optional argument the selection is a ghost
         location), which makes some clients reject the whole response. Fall
         back to [range] when containment does not hold. *)
      let selectionRange =
        let selectionRange = Range.of_loc selection in
        if Range.contains range selectionRange then selectionRange else range
      in
      DocumentSymbol.create
        ~name:outline_name
        ~kind:(symbol_kind_of_outline_kind outline_kind)
        ~range
        ~selectionRange
        ~children:(items_to_symbols children)
        ())
    items
;;

let rec flatten_document_symbols ~uri ~container_name (symbols : DocumentSymbol.t list) =
  List.concat_map symbols ~f:(fun symbol ->
    let symbol_information =
      SymbolInformation.create
        ?containerName:container_name
        ~kind:symbol.kind
        ~location:{ range = symbol.range; uri }
        ~name:symbol.name
        ()
    in
    let children =
      flatten_document_symbols
        ~uri
        ~container_name:(Some symbol.name)
        (Option.value symbol.children ~default:[])
    in
    symbol_information :: children)
;;

let run (client_capabilities : ClientCapabilities.t) doc uri =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let+ outline =
      Document.Merlin.with_pipeline_exn ~name:"document-symbols" merlin (fun pipeline ->
        Query_commands.dispatch pipeline Query_protocol.Outline)
    in
    let symbols = items_to_symbols outline in
    (match
       Option.value
         ~default:false
         (let open Option.O in
          let* textDocument = client_capabilities.textDocument in
          let* ds = textDocument.documentSymbol in
          ds.hierarchicalDocumentSymbolSupport)
     with
     | true -> Some (`DocumentSymbol symbols)
     | false ->
       let flattened = flatten_document_symbols ~uri ~container_name:None symbols in
       Some (`SymbolInformation flattened))
;;
