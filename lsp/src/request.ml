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
  | UnknownRequest : string * Yojson.Safe.t -> unit t

let yojson_of_result (type a) (req : a t) (result : a) =
  match (req, result) with
  | Shutdown, () -> None
  | TextDocumentHover _, result -> Some (Hover.yojson_of_result result)
  | TextDocumentDefinition _, result ->
    Some (Definition.yojson_of_result result)
  | TextDocumentTypeDefinition _, result ->
    Some (TypeDefinition.yojson_of_result result)
  | TextDocumentCompletion _, result ->
    Some (Completion.yojson_of_result result)
  | TextDocumentCodeLens _, result -> Some (CodeLens.yojson_of_result result)
  | TextDocumentRename _, result -> Some (Rename.yojson_of_result result)
  | DocumentSymbol _, result ->
    Some (TextDocumentDocumentSymbol.yojson_of_result result)
  | DebugEcho _, result -> Some (DebugEcho.yojson_of_result result)
  | DebugTextDocumentGet _, result ->
    Some (DebugTextDocumentGet.yojson_of_result result)
  | TextDocumentReferences _, result ->
    Some (References.yojson_of_result result)
  | TextDocumentHighlight _, result ->
    Some (TextDocumentHighlight.yojson_of_result result)
  | TextDocumentFoldingRange _, result ->
    Some (FoldingRange.yojson_of_result result)
  | SignatureHelp _, result -> Some (SignatureHelp.yojson_of_t result)
  | CodeAction _, result -> Some (CodeAction.yojson_of_result result)
  | CompletionItemResolve _, result ->
    Some (Completion.yojson_of_completionItem result)
  | UnknownRequest _, _resp -> None
