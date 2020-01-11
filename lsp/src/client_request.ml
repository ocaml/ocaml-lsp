open! Import
open Protocol

type _ t =
  | Shutdown : unit t
  | Initialize : Initialize.Params.t -> Initialize.Result.t t
  | TextDocumentHover : Hover.params -> Hover.result t
  | TextDocumentDefinition : Definition.params -> Definition.result t
  | TextDocumentTypeDefinition :
      TypeDefinition.params
      -> TypeDefinition.result t
  | TextDocumentCompletion : Completion.params -> Completion.result t
  | TextDocumentCodeLens : CodeLens.params -> CodeLens.result t
  | TextDocumentPrepareRename :
      TextDocumentPositionParams.t
      -> PrepareRename.Result.t t
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
  | WillSaveWaitUntilTextDocument :
      WillSaveTextDocumentParams.t
      -> WillSaveWaitUntilTextDocument.Result.t t
  | TextDocumentFormatting :
      DocumentFormattingParams.t
      -> TextDocumentFormatting.Result.t t
  | TextDocumentOnTypeFormatting :
      DocumentOnTypeFormattingParams.t
      -> TextDocumentOnTypeFormatting.Result.t t
  | UnknownRequest : string * Json.t option -> unit t

let yojson_of_result (type a) (req : a t) (result : a) =
  match (req, result) with
  | Shutdown, () -> None
  | Initialize _, result -> Some (Initialize.Result.yojson_of_t result)
  | TextDocumentHover _, result -> Some (Hover.yojson_of_result result)
  | TextDocumentDefinition _, result ->
    Some (Definition.yojson_of_result result)
  | TextDocumentTypeDefinition _, result ->
    Some (TypeDefinition.yojson_of_result result)
  | TextDocumentCompletion _, result ->
    Some (Completion.yojson_of_result result)
  | TextDocumentCodeLens _, result -> Some (CodeLens.yojson_of_result result)
  | TextDocumentPrepareRename _, result ->
    Some (PrepareRename.Result.yojson_of_t result)
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
  | WillSaveWaitUntilTextDocument _, result ->
    Some (WillSaveWaitUntilTextDocument.Result.yojson_of_t result)
  | TextDocumentOnTypeFormatting _, result ->
    Some (TextDocumentOnTypeFormatting.Result.yojson_of_t result)
  | TextDocumentFormatting _, result ->
    Some (TextDocumentFormatting.Result.yojson_of_t result)
  | UnknownRequest _, _resp -> None

type packed = E : 'r t -> packed

let of_jsonrpc (r : Jsonrpc.Request.t) =
  let open Result.Infix in
  let parse f = Jsonrpc.Request.params r f in
  match r.method_ with
  | "initialize" ->
    parse Initialize.Params.t_of_yojson >>| fun params -> E (Initialize params)
  | "shutdown" -> Ok (E Shutdown)
  | "textDocument/completion" ->
    parse Completion.params_of_yojson >>| fun params ->
    E (TextDocumentCompletion params)
  | "textDocument/documentSymbol" ->
    parse TextDocumentDocumentSymbol.params_of_yojson >>| fun params ->
    E (DocumentSymbol params)
  | "textDocument/hover" ->
    parse Hover.params_of_yojson >>| fun params -> E (TextDocumentHover params)
  | "textDocument/definition" ->
    parse Definition.params_of_yojson >>| fun params ->
    E (TextDocumentDefinition params)
  | "textDocument/typeDefinition" ->
    parse TypeDefinition.params_of_yojson >>| fun params ->
    E (TextDocumentTypeDefinition params)
  | "textDocument/references" ->
    parse References.params_of_yojson >>| fun params ->
    E (TextDocumentReferences params)
  | "textDocument/codeLens" ->
    parse CodeLens.params_of_yojson >>| fun params ->
    E (TextDocumentCodeLens params)
  | "textDocument/rename" ->
    parse Rename.params_of_yojson >>| fun params ->
    E (TextDocumentRename params)
  | "textDocument/documentHighlight" ->
    parse TextDocumentHighlight.params_of_yojson >>| fun params ->
    E (TextDocumentHighlight params)
  | "textDocument/foldingRange" ->
    parse FoldingRange.params_of_yojson >>| fun params ->
    E (TextDocumentFoldingRange params)
  | "textDocument/codeAction" ->
    parse CodeActionParams.t_of_yojson >>| fun params -> E (CodeAction params)
  | "debug/echo" ->
    parse DebugEcho.params_of_yojson >>| fun params -> E (DebugEcho params)
  | "debug/textDocument/get" ->
    parse DebugTextDocumentGet.params_of_yojson >>| fun params ->
    E (DebugTextDocumentGet params)
  | m -> Ok (E (UnknownRequest (m, r.params)))
