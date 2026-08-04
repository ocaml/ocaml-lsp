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

let rec items_to_symbols ~supports_deprecated_tag items =
  List.rev_map
    ~f:
      (fun
        { Query_protocol.outline_name
        ; outline_kind
        ; location
        ; selection
        ; children
        ; deprecated
        ; _
        } ->
      let range = Range.of_loc location in
      (* The LSP spec requires [selectionRange] to be contained in [range].
         Preserve valid selections, clip non-empty overlaps, and fall back to
         [range] for ghost, invalid, touching, or disjoint selections. *)
      let selectionRange =
        match Range.of_loc_opt selection with
        | None -> range
        | Some selection -> Lsp.Range.normalize_selection_range range ~selection
      in
      let { Deprecation.deprecated; tags } =
        Deprecation.create
          ~deprecated
          ~tag:SymbolTag.Deprecated
          ~supports_tag:supports_deprecated_tag
          ~supports_deprecated_field:true
      in
      DocumentSymbol.create
        ~name:outline_name
        ~kind:(symbol_kind_of_outline_kind outline_kind)
        ~range
        ~selectionRange
        ?deprecated
        ?tags
        ~children:(items_to_symbols ~supports_deprecated_tag children)
        ())
    items
;;

let run (client_capabilities : ClientCapabilities.t) doc uri =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let+ outline =
      Document.Merlin.with_pipeline_exn ~name:"document-symbols" merlin (fun pipeline ->
        Query_commands.dispatch pipeline Query_protocol.Outline)
    in
    let document_symbol_capabilities =
      let open Option.O in
      let* text_document = client_capabilities.textDocument in
      text_document.documentSymbol
    in
    let supports_deprecated_tag =
      Option.value
        ~default:false
        (let open Option.O in
         let* document_symbol = document_symbol_capabilities in
         let* tag_support = document_symbol.tagSupport in
         Some
           (Deprecation.tag_supported
              tag_support.valueSet
              ~tag:Deprecated
              ~equal:(fun SymbolTag.Deprecated Deprecated -> true)))
    in
    let symbols = items_to_symbols ~supports_deprecated_tag outline in
    (match
       Option.value
         ~default:false
         (let open Option.O in
          let* document_symbol = document_symbol_capabilities in
          document_symbol.hierarchicalDocumentSymbolSupport)
     with
     | true -> Some (`DocumentSymbol symbols)
     | false ->
       let flattened = Lsp.Document_symbol.flatten ~uri symbols in
       Some (`SymbolInformation flattened))
;;
