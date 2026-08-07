(** Accessors for client capabilities.

    The client capability tree is a deep structure of optional records. These
    functions collapse the navigation into single lookups. Boolean flags
    follow the convention that an absent capability is treated as [false]. *)

open Types

type t = ClientCapabilities.t

(* Completion *)

(** The formats accepted for completion item documentation. *)
val completion_documentation_format : t -> MarkupKind.t list option

(** Whether the client supports the deprecated field of completion items. *)
val completion_deprecated_support : t -> bool

(** Whether the client supports preselecting a completion item. *)
val completion_preselect_support : t -> bool

(** The properties the client supports resolving for completion items. *)
val completion_resolve_properties : t -> string list option

(** The set of completion item tags the client supports. *)
val completion_tag_support : t -> CompletionItemTag.t list option

(** The set of completion item kinds the client supports. *)
val completion_item_kind_support : t -> CompletionItemKind.t list option

(* Document symbol *)

(** Whether the client supports hierarchical document symbols. *)
val document_symbol_hierarchical_support : t -> bool

(** The set of document symbol tags the client supports. *)
val document_symbol_tag_support : t -> SymbolTag.t list option

(** The set of document symbol kinds the client supports. When absent, the
    client only supports the kinds from the initial version of the protocol,
    [File] to [Array]. *)
val document_symbol_kind_support : t -> SymbolKind.t list option

(* Signature help *)

(** Whether the client supports offset-based parameter labels. *)
val signature_label_offset_support : t -> bool

(** The formats accepted for signature help documentation. *)
val signature_documentation_format : t -> MarkupKind.t list option

(* Hover *)

(** The formats accepted for hover contents. *)
val hover_content_format : t -> MarkupKind.t list option

(* Code actions *)

val code_action_literal_support : t -> ClientCodeActionLiteralOptions.t option

(** Whether the client supports the data field of code actions. *)
val code_action_data_support : t -> bool

(** The properties the client supports resolving for code actions. *)
val code_action_resolve_properties : t -> string list option

(* Semantic tokens *)

val semantic_tokens : t -> SemanticTokensClientCapabilities.t option

(* Folding range *)

val folding_range : t -> FoldingRangeClientCapabilities.t option

(** Whether the client supports folding only whole lines. *)
val folding_range_line_folding_only : t -> bool

(** The set of folding range kinds the client supports. *)
val folding_range_kinds : t -> FoldingRangeKind.t list option

(** The maximum number of folding ranges the client accepts. *)
val folding_range_limit : t -> int option

(* Publish diagnostics *)

val publish_diagnostics : t -> PublishDiagnosticsClientCapabilities.t option

(** Whether the client supports related information in diagnostics. *)
val publish_diagnostics_related_information_support : t -> bool

(** The set of diagnostic tags the client supports. *)
val publish_diagnostics_tag_support : t -> DiagnosticTag.t list option

(** Whether the client supports the data field of diagnostics. *)
val publish_diagnostics_data_support : t -> bool

(* Text document synchronization *)

(** Whether the client supports dynamic registration of text document
    synchronization. *)
val text_document_sync_dynamic_registration : t -> bool

(* Workspace *)

(** The set of workspace-symbol tags the client supports. *)
val workspace_symbol_tag_support : t -> SymbolTag.t list option

(** Whether the client supports document changes in workspace edits. *)
val workspace_edit_document_changes : t -> bool

(* Window *)

val show_document : t -> ShowDocumentClientCapabilities.t option

(** Whether the client supports work done progress. *)
val work_done_progress : t -> bool

(* General *)

val position_encodings : t -> PositionEncodingKind.t list option

(* Helpers *)

(** Whether Markdown is the client's preferred markup format. *)
val supports_markdown : MarkupKind.t list option -> bool

(** Whether the client's advertised value set includes [tag]. *)
val supported : 'a list -> tag:'a -> equal:('a -> 'a -> bool) -> bool
