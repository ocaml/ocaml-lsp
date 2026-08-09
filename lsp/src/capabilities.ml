open Import
open Types

(* [Option.map] with the option argument first, so that the function's
     argument type is known while it is typechecked. This is required to
     resolve record labels of types from [Types], which has an interface. *)
let map opt f = Option.map f opt

type t = ClientCapabilities.t

(* Completion *)

let completion (t : t) = Option.bind t.textDocument (fun td -> td.completion)
let completion_item (t : t) = Option.bind (completion t) (fun c -> c.completionItem)

let completion_documentation_format (t : t) =
  Option.bind (completion_item t) (fun item -> item.documentationFormat)
;;

let completion_deprecated_support (t : t) =
  Option.bind (completion_item t) (fun item -> item.deprecatedSupport)
  |> Option.value ~default:false
;;

let completion_preselect_support (t : t) =
  Option.bind (completion_item t) (fun item -> item.preselectSupport)
  |> Option.value ~default:false
;;

let completion_resolve_properties (t : t) =
  Option.bind (completion_item t) (fun item ->
    map item.resolveSupport (fun resolve -> resolve.properties))
;;

let completion_tag_support (t : t) =
  Option.bind (completion_item t) (fun item ->
    map item.tagSupport (fun tag_support -> tag_support.valueSet))
;;

let completion_item_kind_support (t : t) =
  Option.bind (completion t) (fun c ->
    Option.bind c.completionItemKind (fun kinds -> kinds.valueSet))
;;

(* Document symbol *)

let document_symbol (t : t) = Option.bind t.textDocument (fun td -> td.documentSymbol)

let document_symbol_hierarchical_support (t : t) =
  Option.bind (document_symbol t) (fun ds -> ds.hierarchicalDocumentSymbolSupport)
  |> Option.value ~default:false
;;

let document_symbol_tag_support (t : t) =
  Option.bind (document_symbol t) (fun ds ->
    map ds.tagSupport (fun tag_support -> tag_support.valueSet))
;;

let document_symbol_kind_support (t : t) =
  Option.bind (document_symbol t) (fun ds ->
    Option.bind ds.symbolKind (fun symbol_kind -> symbol_kind.valueSet))
;;

(* Signature help *)

let signature_help (t : t) = Option.bind t.textDocument (fun td -> td.signatureHelp)

let signature_label_offset_support (t : t) =
  Option.bind (signature_help t) (fun sh ->
    Option.bind sh.signatureInformation (fun si ->
      Option.bind si.parameterInformation (fun pi -> pi.labelOffsetSupport)))
  |> Option.value ~default:false
;;

let signature_documentation_format (t : t) =
  Option.bind (signature_help t) (fun sh ->
    Option.bind sh.signatureInformation (fun si -> si.documentationFormat))
;;

(* Hover *)

let hover_content_format (t : t) =
  Option.bind t.textDocument (fun td -> Option.bind td.hover (fun h -> h.contentFormat))
;;

(* Code actions *)

let code_action (t : t) = Option.bind t.textDocument (fun td -> td.codeAction)

let code_action_literal_support (t : t) =
  Option.bind (code_action t) (fun ca -> ca.codeActionLiteralSupport)
;;

let code_action_data_support (t : t) =
  Option.bind (code_action t) (fun ca -> ca.dataSupport) |> Option.value ~default:false
;;

let code_action_resolve_properties (t : t) =
  Option.bind (code_action t) (fun ca ->
    map ca.resolveSupport (fun resolve -> resolve.properties))
;;

(* Semantic tokens *)

let semantic_tokens (t : t) = Option.bind t.textDocument (fun td -> td.semanticTokens)

(* Folding range *)

let folding_range (t : t) = Option.bind t.textDocument (fun td -> td.foldingRange)

let folding_range_line_folding_only (t : t) =
  Option.bind (folding_range t) (fun fr -> fr.lineFoldingOnly)
  |> Option.value ~default:false
;;

let folding_range_kinds (t : t) =
  Option.bind (folding_range t) (fun fr ->
    Option.bind fr.foldingRangeKind (fun kinds -> kinds.valueSet))
;;

let folding_range_limit (t : t) = Option.bind (folding_range t) (fun fr -> fr.rangeLimit)

(* Publish diagnostics *)

let publish_diagnostics (t : t) =
  Option.bind t.textDocument (fun td -> td.publishDiagnostics)
;;

let publish_diagnostics_related_information_support (t : t) =
  Option.bind (publish_diagnostics t) (fun pd -> pd.relatedInformation)
  |> Option.value ~default:false
;;

let publish_diagnostics_tag_support (t : t) =
  Option.bind (publish_diagnostics t) (fun pd ->
    map pd.tagSupport (fun tag_support -> tag_support.valueSet))
;;

let publish_diagnostics_data_support (t : t) =
  Option.bind (publish_diagnostics t) (fun pd -> pd.dataSupport)
  |> Option.value ~default:false
;;

(* Text document synchronization *)

let text_document_sync_dynamic_registration (t : t) =
  Option.bind t.textDocument (fun td ->
    Option.bind td.synchronization (fun sync -> sync.dynamicRegistration))
  |> Option.value ~default:false
;;

(* Workspace *)

let workspace_symbol_tag_support (t : t) =
  Option.bind t.workspace (fun w ->
    Option.bind w.symbol (fun symbol ->
      map symbol.tagSupport (fun tag_support -> tag_support.valueSet)))
;;

let workspace_edit_document_changes (t : t) =
  Option.bind t.workspace (fun w ->
    Option.bind w.workspaceEdit (fun edit -> edit.documentChanges))
  |> Option.value ~default:false
;;

(* Window *)

let show_document (t : t) = Option.bind t.window (fun w -> w.showDocument)

let work_done_progress (t : t) =
  Option.bind t.window (fun w -> w.workDoneProgress) |> Option.value ~default:false
;;

(* General *)

let position_encodings (t : t) = Option.bind t.general (fun g -> g.positionEncodings)

(* Helpers *)

let supports_markdown = function
  | Some (MarkupKind.Markdown :: _) -> true
  | _ -> false
;;

let supported xs ~tag ~equal = List.exists ~f:(equal tag) xs
