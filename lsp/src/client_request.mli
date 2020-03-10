open! Import
open Protocol
module InitializeParams = Gprotocol.InitializeParams
module InitializeResult = Gprotocol.InitializeResult
module CodeActionParams = Gprotocol.CodeActionParams
module CodeActionResult = Gprotocol.CodeActionResult
module ExecuteCommandParams = Gprotocol.ExecuteCommandParams

type _ t =
  | Shutdown : unit t
  | Initialize : InitializeParams.t -> InitializeResult.t t
  | TextDocumentHover : Hover.params -> Hover.result t
  | TextDocumentDefinition : Definition.params -> Definition.result t
  | TextDocumentDeclaration :
      TextDocumentPositionParams.t
      -> Locations.t option t
  | TextDocumentTypeDefinition :
      TypeDefinition.params
      -> TypeDefinition.result t
  | TextDocumentCompletion : Completion.params -> Completion.result t
  | TextDocumentCodeLens : CodeLens.Params.t -> CodeLens.Result.t t
  | TextDocumentCodeLensResolve : CodeLens.t -> CodeLens.t t
  | TextDocumentPrepareRename :
      TextDocumentPositionParams.t
      -> PrepareRename.Result.t t
  | TextDocumentRename : Rename.params -> Rename.result t
  | TextDocumentLink : DocumentLink.Params.t -> DocumentLink.Result.t t
  | TextDocumentLinkResolve : DocumentLink.t -> DocumentLink.t t
  | DocumentSymbol :
      TextDocumentDocumentSymbol.params
      -> TextDocumentDocumentSymbol.result t
  | WorkspaceSymbol : WorkspaceSymbol.Params.t -> WorkspaceSymbol.Result.t t
  | DebugEcho : DebugEcho.params -> DebugEcho.result t
  | DebugTextDocumentGet :
      DebugTextDocumentGet.params
      -> DebugTextDocumentGet.result t
  | TextDocumentReferences : References.params -> References.result t
  | TextDocumentHighlight :
      TextDocumentHighlight.params
      -> TextDocumentHighlight.result t
  | TextDocumentFoldingRange : FoldingRange.params -> FoldingRange.result t
  | SignatureHelp : TextDocumentPositionParams.t -> SignatureHelp.t t
  | CodeAction : CodeActionParams.t -> CodeActionResult.t t
  | CompletionItemResolve :
      Completion.completionItem
      -> Completion.completionItem t
  | WillSaveWaitUntilTextDocument :
      WillSaveTextDocumentParams.t
      -> WillSaveWaitUntilTextDocument.Result.t t
  | TextDocumentFormatting : DocumentFormattingParams.t -> TextEdit.t list t
  | TextDocumentOnTypeFormatting :
      DocumentOnTypeFormattingParams.t
      -> TextDocumentOnTypeFormatting.Result.t t
  | TextDocumentColorPresentation :
      ColorPresentation.Params.t
      -> ColorPresentation.t list t
  | TextDocumentColor : DocumentColor.Params.t -> DocumentColor.Result.t t
  | SelectionRange : SelectionRange.Params.t -> SelectionRange.t list t
  | ExecuteCommand : ExecuteCommandParams.t -> Json.t t
  | UnknownRequest : string * Json.t option -> unit t

val yojson_of_result : 'a t -> 'a -> Json.t option

type packed = E : 'r t -> packed

val of_jsonrpc : Jsonrpc.Request.t -> (packed, string) Result.t
