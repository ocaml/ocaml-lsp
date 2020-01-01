open! Import
open Protocol

type _ t =
  | Shutdown : unit t
  | TextDocumentHover : Hover.params -> Hover.result t
  | TextDocumentDefinition : Definition.params -> Definition.result t
  | TextDocumentTypeDefinition :
      TypeDefinition.params
      -> TypeDefinition.result t
  | TextDocumentCompletion : Completion.params -> Completion.result t
  | TextDocumentCodeLens : CodeLens.params -> CodeLens.result t
  | TextDocumentRename : Rename.params -> Rename.result t
  | DocumentSymbol :
      TextDocumentDocumentSymbol.params
      -> TextDocumentDocumentSymbol.result t
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
  | CodeAction : CodeActionParams.t -> CodeAction.result t
  | CompletionItemResolve :
      Completion.completionItem
      -> Completion.completionItem t
  | UnknownRequest : string * json option -> unit t

val yojson_of_result : 'a t -> 'a -> json option
