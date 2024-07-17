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

let yojson_of_DocumentSymbol ds : Json.t =
  Json.Option.yojson_of_t
    (function
      | `DocumentSymbol ds -> Json.To.list DocumentSymbol.yojson_of_t ds
      | `SymbolInformation si -> Json.To.list SymbolInformation.yojson_of_t si)
    ds
;;

let yojson_of_Completion ds : Json.t =
  Json.Option.yojson_of_t
    (function
      | `CompletionList cs -> CompletionList.yojson_of_t cs
      | `List xs -> `List (List.map xs ~f:CompletionItem.yojson_of_t))
    ds
;;

let yojson_of_SemanticTokensDelta ds : Json.t =
  Json.Option.yojson_of_t
    (function
      | `SemanticTokens st -> SemanticTokens.yojson_of_t st
      | `SemanticTokensDelta st -> SemanticTokensDelta.yojson_of_t st)
    ds
;;

let yojson_of_TextDocumentInlineCompletion t : Json.t =
  Json.Option.yojson_of_t
    (function
      | `InlineCompletion t -> InlineCompletionList.yojson_of_t t
      | `InlineCompletionItem t -> `List (List.map ~f:InlineCompletionItem.yojson_of_t t))
    t
;;

let yojson_of_result (type a) (req : a t) (result : a) =
  match req, result with
  | Shutdown, () -> `Null
  | Initialize _, result -> InitializeResult.yojson_of_t result
  | TextDocumentDeclaration _, result ->
    Json.Conv.yojson_of_option Locations.yojson_of_t result
  | TextDocumentHover _, result -> Json.Option.yojson_of_t Hover.yojson_of_t result
  | TextDocumentDefinition _, result ->
    Json.Option.yojson_of_t Locations.yojson_of_t result
  | TextDocumentTypeDefinition _, result ->
    Json.Option.yojson_of_t Locations.yojson_of_t result
  | TextDocumentImplementation _, result ->
    Json.Option.yojson_of_t Locations.yojson_of_t result
  | TextDocumentCompletion _, result -> yojson_of_Completion result
  | TextDocumentCodeLens _, result -> Json.To.list CodeLens.yojson_of_t result
  | TextDocumentCodeLensResolve _, result -> CodeLens.yojson_of_t result
  | TextDocumentPrepareCallHierarchy _, result ->
    Json.Option.yojson_of_t (Json.To.list CallHierarchyItem.yojson_of_t) result
  | TextDocumentPrepareTypeHierarchy _, result ->
    Json.Option.yojson_of_t (Json.To.list TypeHierarchyItem.yojson_of_t) result
  | TextDocumentPrepareRename _, result ->
    Json.Option.yojson_of_t Range.yojson_of_t result
  | TextDocumentRangeFormatting _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentRangesFormatting _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentRename _, result -> WorkspaceEdit.yojson_of_t result
  | DocumentSymbol _, result -> yojson_of_DocumentSymbol result
  | DebugEcho _, result -> DebugEcho.Result.yojson_of_t result
  | DebugTextDocumentGet _, result -> DebugTextDocumentGet.Result.yojson_of_t result
  | TextDocumentReferences _, result ->
    Json.Option.yojson_of_t (Json.To.list Location.yojson_of_t) result
  | TextDocumentHighlight _, result ->
    Json.Option.yojson_of_t (Json.To.list DocumentHighlight.yojson_of_t) result
  | TextDocumentFoldingRange _, result ->
    Json.Option.yojson_of_t (Json.To.list FoldingRange.yojson_of_t) result
  | TextDocumentMoniker _, result ->
    Json.Option.yojson_of_t (Json.To.list Moniker.yojson_of_t) result
  | SignatureHelp _, result -> SignatureHelp.yojson_of_t result
  | CodeAction _, result -> CodeActionResult.yojson_of_t result
  | CodeActionResolve _, result -> CodeAction.yojson_of_t result
  | CompletionItemResolve _, result -> CompletionItem.yojson_of_t result
  | WillSaveWaitUntilTextDocument _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentOnTypeFormatting _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentFormatting _, result ->
    Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result
  | TextDocumentLink _, result ->
    Json.Option.yojson_of_t (Json.To.list DocumentLink.yojson_of_t) result
  | TextDocumentLinkResolve _, result -> DocumentLink.yojson_of_t result
  | WorkspaceSymbol _, result ->
    Json.Option.yojson_of_t (Json.To.list SymbolInformation.yojson_of_t) result
  | TextDocumentColorPresentation _, result ->
    Json.To.list ColorPresentation.yojson_of_t result
  | TextDocumentColor _, result -> Json.To.list ColorInformation.yojson_of_t result
  | SelectionRange _, result -> Json.yojson_of_list SelectionRange.yojson_of_t result
  | SemanticTokensFull _, result ->
    Json.Option.yojson_of_t SemanticTokens.yojson_of_t result
  | SemanticTokensDelta _, result -> yojson_of_SemanticTokensDelta result
  | SemanticTokensRange _, result ->
    Json.Option.yojson_of_t SemanticTokens.yojson_of_t result
  | LinkedEditingRange _, result ->
    Json.Option.yojson_of_t LinkedEditingRanges.yojson_of_t result
  | CallHierarchyIncomingCalls _, result ->
    Json.Option.yojson_of_t (Json.To.list CallHierarchyIncomingCall.yojson_of_t) result
  | CallHierarchyOutgoingCalls _, result ->
    Json.Option.yojson_of_t (Json.To.list CallHierarchyOutgoingCall.yojson_of_t) result
  | WillCreateFiles _, result -> Json.Option.yojson_of_t WorkspaceEdit.yojson_of_t result
  | WillDeleteFiles _, result -> Json.Option.yojson_of_t WorkspaceEdit.yojson_of_t result
  | WillRenameFiles _, result -> Json.Option.yojson_of_t WorkspaceEdit.yojson_of_t result
  | ExecuteCommand _, result -> result
  | InlayHint _, result ->
    Json.Option.yojson_of_t (Json.To.list InlayHint.yojson_of_t) result
  | InlayHintResolve _, result -> InlayHint.yojson_of_t result
  | TextDocumentDiagnostic _, result -> DocumentDiagnosticReport.yojson_of_t result
  | TextDocumentInlineCompletion _, result ->
    yojson_of_TextDocumentInlineCompletion result
  | TextDocumentInlineValue _, result ->
    Json.Option.yojson_of_t (Json.To.list InlineValue.yojson_of_t) result
  | WorkspaceDiagnostic _, result -> WorkspaceDiagnosticReport.yojson_of_t result
  | WorkspaceSymbolResolve _, result -> WorkspaceSymbol.yojson_of_t result
  | TypeHierarchySubtypes _, result ->
    Json.Option.yojson_of_t (Json.To.list TypeHierarchyItem.yojson_of_t) result
  | TypeHierarchySupertypes _, result ->
    Json.Option.yojson_of_t (Json.To.list TypeHierarchyItem.yojson_of_t) result
  | UnknownRequest _, resp -> resp
;;

type packed = E : 'r t -> packed

let of_jsonrpc (r : Jsonrpc.Request.t) =
  let open Result.O in
  let parse f = Json.message_params r.params f in
  match r.method_ with
  | "initialize" ->
    let+ params = parse InitializeParams.t_of_yojson in
    E (Initialize params)
  | "shutdown" -> Ok (E Shutdown)
  | "textDocument/completion" ->
    let+ params = parse CompletionParams.t_of_yojson in
    E (TextDocumentCompletion params)
  | "completionItem/resolve" ->
    let+ params = parse CompletionItem.t_of_yojson in
    E (CompletionItemResolve params)
  | "textDocument/documentSymbol" ->
    let+ params = parse DocumentSymbolParams.t_of_yojson in
    E (DocumentSymbol params)
  | "textDocument/hover" ->
    let+ params = parse HoverParams.t_of_yojson in
    E (TextDocumentHover params)
  | "textDocument/definition" ->
    let+ params = parse DefinitionParams.t_of_yojson in
    E (TextDocumentDefinition params)
  | "textDocument/typeDefinition" ->
    let+ params = parse TypeDefinitionParams.t_of_yojson in
    E (TextDocumentTypeDefinition params)
  | "textDocument/implementation" ->
    let+ params = parse ImplementationParams.t_of_yojson in
    E (TextDocumentImplementation params)
  | "textDocument/references" ->
    let+ params = parse ReferenceParams.t_of_yojson in
    E (TextDocumentReferences params)
  | "textDocument/codeLens" ->
    let+ params = parse CodeLensParams.t_of_yojson in
    E (TextDocumentCodeLens params)
  | "textDocument/inlayHint" ->
    let+ params = parse InlayHintParams.t_of_yojson in
    E (InlayHint params)
  | "textDocument/prepareCallHierarchy" ->
    let+ params = parse CallHierarchyPrepareParams.t_of_yojson in
    E (TextDocumentPrepareCallHierarchy params)
  | "textDocument/prepareRename" ->
    let+ params = parse PrepareRenameParams.t_of_yojson in
    E (TextDocumentPrepareRename params)
  | "textDocument/rangeFormatting" ->
    let+ params = parse DocumentRangeFormattingParams.t_of_yojson in
    E (TextDocumentRangeFormatting params)
  | "textDocument/rangesFormatting" ->
    let+ params = parse DocumentRangesFormattingParams.t_of_yojson in
    E (TextDocumentRangesFormatting params)
  | "textDocument/rename" ->
    let+ params = parse RenameParams.t_of_yojson in
    E (TextDocumentRename params)
  | "textDocument/documentHighlight" ->
    let+ params = parse DocumentHighlightParams.t_of_yojson in
    E (TextDocumentHighlight params)
  | "textDocument/foldingRange" ->
    let+ params = parse FoldingRangeParams.t_of_yojson in
    E (TextDocumentFoldingRange params)
  | "textDocument/signatureHelp" ->
    let+ params = parse SignatureHelpParams.t_of_yojson in
    E (SignatureHelp params)
  | "textDocument/codeAction" ->
    let+ params = parse CodeActionParams.t_of_yojson in
    E (CodeAction params)
  | "codeAction/resolve" ->
    let+ params = parse CodeAction.t_of_yojson in
    E (CodeActionResolve params)
  | "debug/echo" ->
    let+ params = parse DebugEcho.Params.t_of_yojson in
    E (DebugEcho params)
  | "debug/textDocument/get" ->
    let+ params = parse DebugTextDocumentGet.Params.t_of_yojson in
    E (DebugTextDocumentGet params)
  | "textDocument/onTypeFormatting" ->
    let+ params = parse DocumentOnTypeFormattingParams.t_of_yojson in
    E (TextDocumentOnTypeFormatting params)
  | "textDocument/formatting" ->
    let+ params = parse DocumentFormattingParams.t_of_yojson in
    E (TextDocumentFormatting params)
  | "textDocument/documentLink" ->
    let+ params = parse DocumentLinkParams.t_of_yojson in
    E (TextDocumentLink params)
  | "documentLink/resolve" ->
    let+ params = parse DocumentLink.t_of_yojson in
    E (TextDocumentLinkResolve params)
  | "workspace/symbol" ->
    let+ params = parse WorkspaceSymbolParams.t_of_yojson in
    E (WorkspaceSymbol params)
  | "textDocument/colorPresentation" ->
    let+ params = parse ColorPresentationParams.t_of_yojson in
    E (TextDocumentColorPresentation params)
  | "textDocument/documentColor" ->
    let+ params = parse DocumentColorParams.t_of_yojson in
    E (TextDocumentColor params)
  | "textDocument/declaration" ->
    let+ params = parse TextDocumentPositionParams.t_of_yojson in
    E (TextDocumentDeclaration params)
  | "textDocument/selectionRange" ->
    let+ params = parse SelectionRangeParams.t_of_yojson in
    E (SelectionRange params)
  | "workspace/executeCommand" ->
    let+ params = parse ExecuteCommandParams.t_of_yojson in
    E (ExecuteCommand params)
  | "textDocument/semanticTokens/full" ->
    let+ params = parse SemanticTokensParams.t_of_yojson in
    E (SemanticTokensFull params)
  | "textDocument/semanticTokens/full/delta" ->
    let+ params = parse SemanticTokensDeltaParams.t_of_yojson in
    E (SemanticTokensDelta params)
  | "textDocument/semanticTokens/range" ->
    let+ params = parse SemanticTokensRangeParams.t_of_yojson in
    E (SemanticTokensRange params)
  | "textDocument/linkedEditingRange" ->
    let+ params = parse LinkedEditingRangeParams.t_of_yojson in
    E (LinkedEditingRange params)
  | "callHierarchy/incomingCalls" ->
    let+ params = parse CallHierarchyIncomingCallsParams.t_of_yojson in
    E (CallHierarchyIncomingCalls params)
  | "callHierarchy/outgoingCalls" ->
    let+ params = parse CallHierarchyOutgoingCallsParams.t_of_yojson in
    E (CallHierarchyOutgoingCalls params)
  | "workspace/willCreateFiles" ->
    let+ params = parse CreateFilesParams.t_of_yojson in
    E (WillCreateFiles params)
  | "workspace/willDeleteFiles" ->
    let+ params = parse DeleteFilesParams.t_of_yojson in
    E (WillDeleteFiles params)
  | "workspace/willRenameFiles" ->
    let+ params = parse RenameFilesParams.t_of_yojson in
    E (WillRenameFiles params)
  | "textDocument/moniker" ->
    let+ params = parse MonikerParams.t_of_yojson in
    E (TextDocumentMoniker params)
  | "codeLens/resolve" ->
    let+ params = parse CodeLens.t_of_yojson in
    E (TextDocumentCodeLensResolve params)
  | "textDocument/willSaveWaitUntil" ->
    let+ params = parse WillSaveTextDocumentParams.t_of_yojson in
    E (WillSaveWaitUntilTextDocument params)
  | "textDocument/inlineValue" ->
    let+ params = parse InlineValueParams.t_of_yojson in
    E (TextDocumentInlineValue params)
  | "inlayHint/resolve" ->
    let+ params = parse InlayHint.t_of_yojson in
    E (InlayHintResolve params)
  | "textDocument/diagnostic" ->
    let+ params = parse DocumentDiagnosticParams.t_of_yojson in
    E (TextDocumentDiagnostic params)
  | "textDocument/inlineCompletion" ->
    let+ params = parse InlineCompletionParams.t_of_yojson in
    E (TextDocumentInlineCompletion params)
  | "workspace/diagnostic" ->
    let+ params = parse WorkspaceDiagnosticParams.t_of_yojson in
    E (WorkspaceDiagnostic params)
  | "workspaceSymbol/resolve" ->
    let+ params = parse WorkspaceSymbol.t_of_yojson in
    E (WorkspaceSymbolResolve params)
  | "typeHierarchy/supertypes" ->
    let+ params = parse TypeHierarchySupertypesParams.t_of_yojson in
    E (TypeHierarchySupertypes params)
  | "typeHierarchy/subtypes" ->
    let+ params = parse TypeHierarchySubtypesParams.t_of_yojson in
    E (TypeHierarchySubtypes params)
  | meth -> Ok (E (UnknownRequest { meth; params = r.params }))
;;

let method_ (type a) (t : a t) =
  match t with
  | Shutdown -> "shutdown"
  | Initialize _ -> "initialize"
  | TextDocumentCompletion _ -> "textDocument/completion"
  | CompletionItemResolve _ -> "completionItem/resolve"
  | DocumentSymbol _ -> "textDocument/documentSymbol"
  | TextDocumentHover _ -> "textDocument/hover"
  | TextDocumentDefinition _ -> "textDocument/definition"
  | TextDocumentTypeDefinition _ -> "textDocument/typeDefinition"
  | TextDocumentImplementation _ -> "textDocument/implementation"
  | TextDocumentReferences _ -> "textDocument/references"
  | TextDocumentCodeLens _ -> "textDocument/codeLens"
  | TextDocumentCodeLensResolve _ -> "codeLens/resolve"
  | TextDocumentPrepareCallHierarchy _ -> "textDocument/prepareCallHierarchy"
  | TextDocumentPrepareTypeHierarchy _ -> "textDocument/prepareTypeHierarchy"
  | TextDocumentPrepareRename _ -> "textDocument/prepareRename"
  | TextDocumentRangeFormatting _ -> "textDocument/rangeFormatting"
  | TextDocumentRangesFormatting _ -> "textDocument/rangesFormatting"
  | TextDocumentRename _ -> "textDocument/rename"
  | TextDocumentHighlight _ -> "textDocument/documentHighlight"
  | TextDocumentFoldingRange _ -> "textDocument/foldingRange"
  | SignatureHelp _ -> "textDocument/signatureHelp"
  | CodeAction _ -> "textDocument/codeAction"
  | CodeActionResolve _ -> "codeAction/resolve"
  | DebugEcho _ -> "debug/echo"
  | DebugTextDocumentGet _ -> "debug/textDocument/get"
  | TextDocumentOnTypeFormatting _ -> "textDocument/onTypeFormatting"
  | TextDocumentFormatting _ -> "textDocument/formatting"
  | TextDocumentLink _ -> "textDocument/documentLink"
  | TextDocumentLinkResolve _ -> "documentLink/resolve"
  | WorkspaceSymbol _ -> "workspace/symbol"
  | TextDocumentColorPresentation _ -> "textDocument/colorPresentation"
  | TextDocumentColor _ -> "textDocument/documentColor"
  | TextDocumentDeclaration _ -> "textDocument/declaration"
  | SelectionRange _ -> "textDocument/selectionRange"
  | ExecuteCommand _ -> "workspace/executeCommand"
  | SemanticTokensFull _ -> "textDocument/semanticTokens/full"
  | SemanticTokensDelta _ -> "textDocument/semanticTokens/full/delta"
  | SemanticTokensRange _ -> "textDocument/semanticTokens/range"
  | LinkedEditingRange _ -> "textDocument/linkedEditingRange"
  | CallHierarchyIncomingCalls _ -> "callHierarchy/incomingCalls"
  | CallHierarchyOutgoingCalls _ -> "callHierarchy/outgoingCalls"
  | WillCreateFiles _ -> "workspace/willCreateFiles"
  | WillDeleteFiles _ -> "workspace/willDeleteFiles"
  | WillRenameFiles _ -> "workspace/willRenameFiles"
  | TextDocumentMoniker _ -> "textDocument/moniker"
  | WillSaveWaitUntilTextDocument _ -> "textDocument/willSaveWaitUntil"
  | InlayHint _ -> "textDocument/inlayHint"
  | InlayHintResolve _ -> "inlayHint/resolve"
  | TextDocumentDiagnostic _ -> "textDocument/diagnostic"
  | TextDocumentInlineCompletion _ -> "textDocument/inlineCompletion"
  | TextDocumentInlineValue _ -> "textDocument/inlineValue"
  | WorkspaceDiagnostic _ -> "workspace/diagnostic"
  | WorkspaceSymbolResolve _ -> "workspaceSymbol/resolve"
  | TypeHierarchySupertypes _ -> "typeHierarchy/supertypes"
  | TypeHierarchySubtypes _ -> "typeHierarchy/subtypes"
  | UnknownRequest { meth; _ } -> meth
;;

let params =
  let ret x = Some (Jsonrpc.Structured.t_of_yojson x) in
  fun (type a) (t : a t) ->
    match t with
    | Shutdown -> None
    | Initialize params -> ret (InitializeParams.yojson_of_t params)
    | TextDocumentCompletion params -> ret (CompletionParams.yojson_of_t params)
    | CompletionItemResolve params -> ret (CompletionItem.yojson_of_t params)
    | DocumentSymbol params -> ret (DocumentSymbolParams.yojson_of_t params)
    | TextDocumentHover params -> ret (HoverParams.yojson_of_t params)
    | TextDocumentDefinition params -> ret (DefinitionParams.yojson_of_t params)
    | TextDocumentTypeDefinition params -> ret (TypeDefinitionParams.yojson_of_t params)
    | TextDocumentImplementation params -> ret (ImplementationParams.yojson_of_t params)
    | TextDocumentReferences params -> ret (ReferenceParams.yojson_of_t params)
    | TextDocumentCodeLens params -> ret (CodeLensParams.yojson_of_t params)
    | TextDocumentPrepareCallHierarchy params ->
      ret (CallHierarchyPrepareParams.yojson_of_t params)
    | TextDocumentPrepareTypeHierarchy params ->
      ret (TypeHierarchyPrepareParams.yojson_of_t params)
    | TextDocumentPrepareRename params -> ret (PrepareRenameParams.yojson_of_t params)
    | TextDocumentRangeFormatting params ->
      ret (DocumentRangeFormattingParams.yojson_of_t params)
    | TextDocumentRangesFormatting params ->
      ret (DocumentRangesFormattingParams.yojson_of_t params)
    | TextDocumentRename params -> ret (RenameParams.yojson_of_t params)
    | TextDocumentHighlight params -> ret (DocumentHighlightParams.yojson_of_t params)
    | TextDocumentFoldingRange params -> ret (FoldingRangeParams.yojson_of_t params)
    | SignatureHelp params -> ret (SignatureHelpParams.yojson_of_t params)
    | CodeAction params -> ret (CodeActionParams.yojson_of_t params)
    | CodeActionResolve params -> ret (CodeAction.yojson_of_t params)
    | DebugEcho params -> ret (DebugEcho.Params.yojson_of_t params)
    | DebugTextDocumentGet params -> ret (DebugTextDocumentGet.Params.yojson_of_t params)
    | TextDocumentOnTypeFormatting params ->
      ret (DocumentOnTypeFormattingParams.yojson_of_t params)
    | TextDocumentFormatting params -> ret (DocumentFormattingParams.yojson_of_t params)
    | TextDocumentLink params -> ret (DocumentLinkParams.yojson_of_t params)
    | TextDocumentLinkResolve params -> ret (DocumentLink.yojson_of_t params)
    | WorkspaceSymbol params -> ret (WorkspaceSymbolParams.yojson_of_t params)
    | TextDocumentColorPresentation params ->
      ret (ColorPresentationParams.yojson_of_t params)
    | TextDocumentColor params -> ret (DocumentColorParams.yojson_of_t params)
    | TextDocumentDeclaration params ->
      ret (TextDocumentPositionParams.yojson_of_t params)
    | SelectionRange params -> ret (SelectionRangeParams.yojson_of_t params)
    | ExecuteCommand params -> ret (ExecuteCommandParams.yojson_of_t params)
    | SemanticTokensFull params -> ret (SemanticTokensParams.yojson_of_t params)
    | SemanticTokensDelta params -> ret (SemanticTokensDeltaParams.yojson_of_t params)
    | SemanticTokensRange params -> ret (SemanticTokensRangeParams.yojson_of_t params)
    | LinkedEditingRange params -> ret (LinkedEditingRangeParams.yojson_of_t params)
    | CallHierarchyIncomingCalls params ->
      ret (CallHierarchyIncomingCallsParams.yojson_of_t params)
    | CallHierarchyOutgoingCalls params ->
      ret (CallHierarchyOutgoingCallsParams.yojson_of_t params)
    | WillCreateFiles params -> ret (CreateFilesParams.yojson_of_t params)
    | WillDeleteFiles params -> ret (DeleteFilesParams.yojson_of_t params)
    | WillRenameFiles params -> ret (RenameFilesParams.yojson_of_t params)
    | TextDocumentMoniker params -> ret (MonikerParams.yojson_of_t params)
    | TextDocumentCodeLensResolve params -> ret (CodeLens.yojson_of_t params)
    | WillSaveWaitUntilTextDocument params ->
      ret (WillSaveTextDocumentParams.yojson_of_t params)
    | InlayHint params -> ret (InlayHintParams.yojson_of_t params)
    | InlayHintResolve params -> ret (InlayHint.yojson_of_t params)
    | TextDocumentDiagnostic params -> ret (DocumentDiagnosticParams.yojson_of_t params)
    | TextDocumentInlineValue params -> ret (InlineValueParams.yojson_of_t params)
    | TextDocumentInlineCompletion params ->
      ret (InlineCompletionParams.yojson_of_t params)
    | WorkspaceDiagnostic params -> ret (WorkspaceDiagnosticParams.yojson_of_t params)
    | WorkspaceSymbolResolve params -> ret (WorkspaceSymbol.yojson_of_t params)
    | TypeHierarchySubtypes params -> ret (TypeHierarchySubtypesParams.yojson_of_t params)
    | TypeHierarchySupertypes params ->
      ret (TypeHierarchySupertypesParams.yojson_of_t params)
    | UnknownRequest { params; _ } -> params
;;

let to_jsonrpc_request t ~id =
  let method_ = method_ t in
  let params = params t in
  Jsonrpc.Request.create ~id ~method_ ?params ()
;;

let response_of_json (type a) (t : a t) (json : Json.t) : a =
  let open Json.Conv in
  match t with
  | Shutdown -> unit_of_yojson json
  | Initialize _ -> InitializeResult.t_of_yojson json
  | TextDocumentHover _ -> option_of_yojson Hover.t_of_yojson json
  | TextDocumentDefinition _ -> option_of_yojson Locations.t_of_yojson json
  | TextDocumentDeclaration _ -> option_of_yojson Locations.t_of_yojson json
  | TextDocumentTypeDefinition _ -> option_of_yojson Locations.t_of_yojson json
  | TextDocumentImplementation _ -> option_of_yojson Locations.t_of_yojson json
  | TextDocumentCompletion _ ->
    option_of_yojson
      (Json.Of.untagged_union
         "completion_list"
         [ (fun json -> `CompletionList (CompletionList.t_of_yojson json))
         ; (fun json -> `List (list_of_yojson CompletionItem.t_of_yojson json))
         ])
      json
  | TextDocumentCodeLens _ -> list_of_yojson CodeLens.t_of_yojson json
  | TextDocumentCodeLensResolve _ -> CodeLens.t_of_yojson json
  | TextDocumentPrepareCallHierarchy _ ->
    option_of_yojson (list_of_yojson CallHierarchyItem.t_of_yojson) json
  | TextDocumentPrepareRename _ -> option_of_yojson Range.t_of_yojson json
  | TextDocumentRangeFormatting _ ->
    option_of_yojson (list_of_yojson TextEdit.t_of_yojson) json
  | TextDocumentRangesFormatting _ ->
    option_of_yojson (list_of_yojson TextEdit.t_of_yojson) json
  | TextDocumentRename _ -> WorkspaceEdit.t_of_yojson json
  | TextDocumentLink _ -> option_of_yojson (list_of_yojson DocumentLink.t_of_yojson) json
  | TextDocumentLinkResolve _ -> DocumentLink.t_of_yojson json
  | TextDocumentMoniker _ -> option_of_yojson (list_of_yojson Moniker.t_of_yojson) json
  | DocumentSymbol _ ->
    option_of_yojson
      (Json.Of.untagged_union
         "document_symbols"
         [ (fun json -> `DocumentSymbol (list_of_yojson DocumentSymbol.t_of_yojson json))
         ; (fun json ->
             `SymbolInformation (list_of_yojson SymbolInformation.t_of_yojson json))
         ])
      json
  | WorkspaceSymbol _ ->
    option_of_yojson (list_of_yojson SymbolInformation.t_of_yojson) json
  | DebugEcho _ -> DebugEcho.Result.t_of_yojson json
  | DebugTextDocumentGet _ -> DebugTextDocumentGet.Result.t_of_yojson json
  | TextDocumentReferences _ ->
    option_of_yojson (list_of_yojson Location.t_of_yojson) json
  | TextDocumentHighlight _ ->
    option_of_yojson (list_of_yojson DocumentHighlight.t_of_yojson) json
  | TextDocumentFoldingRange _ ->
    option_of_yojson (list_of_yojson FoldingRange.t_of_yojson) json
  | SignatureHelp _ -> SignatureHelp.t_of_yojson json
  | CodeAction _ -> CodeActionResult.t_of_yojson json
  | CodeActionResolve _ -> CodeAction.t_of_yojson json
  | CompletionItemResolve _ -> CompletionItem.t_of_yojson json
  | WillSaveWaitUntilTextDocument _ ->
    option_of_yojson (list_of_yojson TextEdit.t_of_yojson) json
  | TextDocumentFormatting _ ->
    option_of_yojson (list_of_yojson TextEdit.t_of_yojson) json
  | TextDocumentOnTypeFormatting _ ->
    option_of_yojson (list_of_yojson TextEdit.t_of_yojson) json
  | TextDocumentColorPresentation _ -> list_of_yojson ColorPresentation.t_of_yojson json
  | TextDocumentColor _ -> list_of_yojson ColorInformation.t_of_yojson json
  | SelectionRange _ -> list_of_yojson SelectionRange.t_of_yojson json
  | ExecuteCommand _ -> json
  | SemanticTokensFull _ -> option_of_yojson SemanticTokens.t_of_yojson json
  | SemanticTokensDelta _ ->
    option_of_yojson
      (Json.Of.untagged_union
         "semantic_tokens"
         [ (fun json -> `SemanticTokens (SemanticTokens.t_of_yojson json))
         ; (fun json -> `SemanticTokensDelta (SemanticTokensDelta.t_of_yojson json))
         ])
      json
  | SemanticTokensRange _ -> option_of_yojson SemanticTokens.t_of_yojson json
  | LinkedEditingRange _ -> option_of_yojson LinkedEditingRanges.t_of_yojson json
  | CallHierarchyIncomingCalls _ ->
    option_of_yojson (list_of_yojson CallHierarchyIncomingCall.t_of_yojson) json
  | CallHierarchyOutgoingCalls _ ->
    option_of_yojson (list_of_yojson CallHierarchyOutgoingCall.t_of_yojson) json
  | WillCreateFiles _ -> option_of_yojson WorkspaceEdit.t_of_yojson json
  | WillDeleteFiles _ -> option_of_yojson WorkspaceEdit.t_of_yojson json
  | WillRenameFiles _ -> option_of_yojson WorkspaceEdit.t_of_yojson json
  | WorkspaceSymbolResolve _ -> WorkspaceSymbol.t_of_yojson json
  | WorkspaceDiagnostic _ -> WorkspaceDiagnosticReport.t_of_yojson json
  | TypeHierarchySubtypes _ ->
    option_of_yojson (Json.Of.list TypeHierarchyItem.t_of_yojson) json
  | TypeHierarchySupertypes _ ->
    option_of_yojson (Json.Of.list TypeHierarchyItem.t_of_yojson) json
  | InlayHint _ -> option_of_yojson (list_of_yojson InlayHint.t_of_yojson) json
  | InlayHintResolve _ -> InlayHint.t_of_yojson json
  | TextDocumentDiagnostic _ -> DocumentDiagnosticReport.t_of_yojson json
  | TextDocumentInlineCompletion _ ->
    option_of_yojson
      (Json.Of.untagged_union
         "inline_completions"
         [ (fun json -> `InlineCompletion (InlineCompletionList.t_of_yojson json))
         ; (fun json ->
             `InlineCompletionItem (Json.Of.list InlineCompletionItem.t_of_yojson json))
         ])
      json
  | TextDocumentInlineValue _ ->
    option_of_yojson (Json.Of.list InlineValue.t_of_yojson) json
  | TextDocumentPrepareTypeHierarchy _ ->
    option_of_yojson (Json.Of.list TypeHierarchyItem.t_of_yojson) json
  | UnknownRequest _ -> json
;;

let text_document (type a) (t : a t) f : TextDocumentIdentifier.t option =
  match t with
  | CompletionItemResolve _ -> None
  | TextDocumentLinkResolve _ -> None
  | ExecuteCommand _ -> None
  | TextDocumentCodeLensResolve _ -> None
  | WorkspaceSymbol _ -> None
  | DebugEcho _ -> None
  | Shutdown -> None
  | Initialize _ -> None
  | TextDocumentHover r -> Some r.textDocument
  | TextDocumentDefinition r -> Some r.textDocument
  | TextDocumentDeclaration r -> Some r.textDocument
  | TextDocumentTypeDefinition r -> Some r.textDocument
  | TextDocumentImplementation r -> Some r.textDocument
  | TextDocumentCompletion r -> Some r.textDocument
  | TextDocumentCodeLens r -> Some r.textDocument
  | TextDocumentPrepareCallHierarchy r -> Some r.textDocument
  | TextDocumentPrepareTypeHierarchy r -> Some r.textDocument
  | TextDocumentPrepareRename r -> Some r.textDocument
  | TextDocumentRangeFormatting r -> Some r.textDocument
  | TextDocumentRangesFormatting r -> Some r.textDocument
  | TextDocumentRename r -> Some r.textDocument
  | TextDocumentLink r -> Some r.textDocument
  | DocumentSymbol r -> Some r.textDocument
  | DebugTextDocumentGet r -> Some r.textDocument
  | TextDocumentReferences r -> Some r.textDocument
  | TextDocumentHighlight r -> Some r.textDocument
  | TextDocumentFoldingRange r -> Some r.textDocument
  | TextDocumentMoniker r -> Some r.textDocument
  | SignatureHelp r -> Some r.textDocument
  | CodeAction r -> Some r.textDocument
  | CodeActionResolve _ -> None
  | WillSaveWaitUntilTextDocument r -> Some r.textDocument
  | TextDocumentFormatting r -> Some r.textDocument
  | TextDocumentOnTypeFormatting r -> Some r.textDocument
  | TextDocumentColorPresentation r -> Some r.textDocument
  | TextDocumentColor r -> Some r.textDocument
  | SelectionRange r -> Some r.textDocument
  | SemanticTokensFull r -> Some r.textDocument
  | SemanticTokensDelta r -> Some r.textDocument
  | SemanticTokensRange r -> Some r.textDocument
  | LinkedEditingRange r -> Some r.textDocument
  | InlayHint r -> Some r.textDocument
  | TextDocumentDiagnostic p -> Some p.textDocument
  | TextDocumentInlineCompletion p -> Some p.textDocument
  | TextDocumentInlineValue p -> Some p.textDocument
  | TypeHierarchySubtypes _ -> None
  | TypeHierarchySupertypes _ -> None
  | WorkspaceSymbolResolve _ -> None
  | WorkspaceDiagnostic _ -> None
  | InlayHintResolve _ -> None
  | CallHierarchyIncomingCalls _ -> None
  | CallHierarchyOutgoingCalls _ -> None
  | WillCreateFiles _ -> None
  | WillDeleteFiles _ -> None
  | WillRenameFiles _ -> None
  | UnknownRequest { meth; params } -> f ~meth ~params
;;
