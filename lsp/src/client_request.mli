open! Import
open Types
open Extension

type _ t =
  | Shutdown : unit t
  | Initialize : InitializeParams.t -> InitializeResult.t t
  | TextDocumentHover : HoverParams.t -> Hover.t option t
  | TextDocumentDefinition : DefinitionParams.t -> Locations.t option t
  | TextDocumentDeclaration : TextDocumentPositionParams.t -> Locations.t option t
  | TextDocumentTypeDefinition : TypeDefinitionParams.t -> Locations.t option t
  | TextDocumentImplementation : ImplementationParams.t -> Locations.t option t
  | TextDocumentCompletion :
      CompletionParams.t
      -> [ `CompletionList of CompletionList.t | `List of CompletionItem.t list ] option t
  | TextDocumentCodeLens : CodeLensParams.t -> CodeLens.t list t
  | InlayHint : InlayHintParams.t -> InlayHint.t list option t
  | InlayHintResolve : InlayHint.t -> InlayHint.t t
  | TextDocumentDiagnostic : DocumentDiagnosticParams.t -> DocumentDiagnosticReport.t t
  | TextDocumentInlineCompletion :
      InlineCompletionParams.t
      -> [ `InlineCompletion of InlineCompletionList.t
         | `InlineCompletionItem of InlineCompletionItem.t list
         ]
           option
           t
  | TextDocumentInlineValue : InlineValueParams.t -> InlineValue.t list option t
  | TextDocumentCodeLensResolve : CodeLens.t -> CodeLens.t t
  | TextDocumentPrepareCallHierarchy :
      CallHierarchyPrepareParams.t
      -> CallHierarchyItem.t list option t
  | TextDocumentPrepareTypeHierarchy :
      TypeHierarchyPrepareParams.t
      -> TypeHierarchyItem.t list option t
  | TextDocumentPrepareRename : PrepareRenameParams.t -> Range.t option t
  | TextDocumentRangeFormatting :
      DocumentRangeFormattingParams.t
      -> TextEdit.t list option t
  | TextDocumentRangesFormatting :
      DocumentRangesFormattingParams.t
      -> TextEdit.t list option t
  | TextDocumentRename : RenameParams.t -> WorkspaceEdit.t t
  | TextDocumentLink : DocumentLinkParams.t -> DocumentLink.t list option t
  | TextDocumentLinkResolve : DocumentLink.t -> DocumentLink.t t
  | TextDocumentMoniker : MonikerParams.t -> Moniker.t list option t
  | DocumentSymbol :
      DocumentSymbolParams.t
      -> [ `DocumentSymbol of DocumentSymbol.t list
         | `SymbolInformation of SymbolInformation.t list
         ]
           option
           t
  | WorkspaceSymbol : WorkspaceSymbolParams.t -> SymbolInformation.t list option t
  | WorkspaceSymbolResolve : WorkspaceSymbol.t -> WorkspaceSymbol.t t
  | DebugEcho : DebugEcho.Params.t -> DebugEcho.Result.t t
  | DebugTextDocumentGet :
      DebugTextDocumentGet.Params.t
      -> DebugTextDocumentGet.Result.t t
  | TextDocumentReferences : ReferenceParams.t -> Location.t list option t
  | TextDocumentHighlight : DocumentHighlightParams.t -> DocumentHighlight.t list option t
  | TextDocumentFoldingRange : FoldingRangeParams.t -> FoldingRange.t list option t
  | SignatureHelp : SignatureHelpParams.t -> SignatureHelp.t t
  | CodeAction : CodeActionParams.t -> CodeActionResult.t t
  | CodeActionResolve : CodeAction.t -> CodeAction.t t
  | CompletionItemResolve : CompletionItem.t -> CompletionItem.t t
  | WillSaveWaitUntilTextDocument :
      WillSaveTextDocumentParams.t
      -> TextEdit.t list option t
  | TextDocumentFormatting : DocumentFormattingParams.t -> TextEdit.t list option t
  | TextDocumentOnTypeFormatting :
      DocumentOnTypeFormattingParams.t
      -> TextEdit.t list option t
  | TextDocumentColorPresentation :
      ColorPresentationParams.t
      -> ColorPresentation.t list t
  | TextDocumentColor : DocumentColorParams.t -> ColorInformation.t list t
  | SelectionRange : SelectionRangeParams.t -> SelectionRange.t list t
  | ExecuteCommand : ExecuteCommandParams.t -> Json.t t
  | SemanticTokensFull : SemanticTokensParams.t -> SemanticTokens.t option t
  | SemanticTokensDelta :
      SemanticTokensDeltaParams.t
      -> [ `SemanticTokens of SemanticTokens.t
         | `SemanticTokensDelta of SemanticTokensDelta.t
         ]
           option
           t
  | SemanticTokensRange : SemanticTokensRangeParams.t -> SemanticTokens.t option t
  | LinkedEditingRange : LinkedEditingRangeParams.t -> LinkedEditingRanges.t option t
  | CallHierarchyIncomingCalls :
      CallHierarchyIncomingCallsParams.t
      -> CallHierarchyIncomingCall.t list option t
  | CallHierarchyOutgoingCalls :
      CallHierarchyOutgoingCallsParams.t
      -> CallHierarchyOutgoingCall.t list option t
  | WillCreateFiles : CreateFilesParams.t -> WorkspaceEdit.t option t
  | WillDeleteFiles : DeleteFilesParams.t -> WorkspaceEdit.t option t
  | WillRenameFiles : RenameFilesParams.t -> WorkspaceEdit.t option t
  | WorkspaceDiagnostic : WorkspaceDiagnosticParams.t -> WorkspaceDiagnosticReport.t t
  | TypeHierarchySubtypes :
      TypeHierarchySubtypesParams.t
      -> TypeHierarchyItem.t list option t
  | TypeHierarchySupertypes :
      TypeHierarchySupertypesParams.t
      -> TypeHierarchyItem.t list option t
  | UnknownRequest :
      { meth : string
      ; params : Jsonrpc.Structured.t option
      }
      -> Json.t t

val yojson_of_result : 'a t -> 'a -> Json.t

type packed = E : 'r t -> packed

val of_jsonrpc : Jsonrpc.Request.t -> (packed, string) Result.t
val to_jsonrpc_request : _ t -> id:Jsonrpc.Id.t -> Jsonrpc.Request.t
val response_of_json : 'a t -> Json.t -> 'a

val text_document
  :  _ t
  -> (meth:string
      -> params:Jsonrpc.Structured.t option
      -> TextDocumentIdentifier.t option)
  -> TextDocumentIdentifier.t option
