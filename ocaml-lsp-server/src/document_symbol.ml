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

let normalize_selection_range ~(range : Range.t) = function
  | None -> range
  | Some (selection_range : Range.t) ->
    let selection_is_valid =
      Lsp.Position.compare selection_range.start selection_range.end_ <= 0
    in
    if selection_is_valid && Lsp.Range.contains range selection_range
    then selection_range
    else Option.value (Lsp.Range.intersection range selection_range) ~default:range
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
        normalize_selection_range ~range (Range.of_loc_opt selection)
      in
      let { Deprecation.deprecated; tags } =
        Deprecation.create
          ~deprecated
          ~tag:Lsp.Types.SymbolTag.Deprecated
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

let rec flatten_document_symbols ~uri ~container_name (symbols : DocumentSymbol.t list) =
  List.concat_map symbols ~f:(fun symbol ->
    let symbol_information =
      SymbolInformation.create
        ?containerName:container_name
        ~kind:symbol.kind
        ~location:{ range = symbol.range; uri }
        ~name:symbol.name
        ?deprecated:symbol.deprecated
        ?tags:symbol.tags
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
              ~tag:Lsp.Types.SymbolTag.Deprecated))
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
       let flattened = flatten_document_symbols ~uri ~container_name:None symbols in
       Some (`SymbolInformation flattened))
;;
