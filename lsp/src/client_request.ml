open! Import
open Protocol
module InitializeParams = Gprotocol.InitializeParams
module InitializeResult = Gprotocol.InitializeResult
module CodeActionResult = Gprotocol.CodeActionResult
module CodeActionParams = Gprotocol.CodeActionParams
module ExecuteCommandParams = Gprotocol.ExecuteCommandParams
module SelectionRangeParams = Gprotocol.SelectionRangeParams
module SelectionRange = Gprotocol.SelectionRange
module DocumentColorParams = Gprotocol.DocumentColorParams
module ColorInformation = Gprotocol.ColorInformation
module CodeLensParams = Gprotocol.CodeLensParams
module CodeLens = Gprotocol.CodeLens
module HoverParams = Gprotocol.HoverParams
module Hover = Gprotocol.Hover
module WorkspaceSymbolParams = Gprotocol.WorkspaceSymbolParams
module SymbolInformation = Gprotocol.SymbolInformation
module DocumentSymbolParams = Gprotocol.DocumentSymbolParams
module DocumentSymbol = Gprotocol.DocumentSymbol
module PrepareRenameParams = Gprotocol.PrepareRenameParams
module RenameParams = Gprotocol.RenameParams
module WorkspaceEdit = Gprotocol.WorkspaceEdit
module WillSaveTextDocumentParams = Gprotocol.WillSaveTextDocumentParams
module TypeDefinitionParams = Gprotocol.TypeDefinitionParams
module Locations = Gprotocol.Locations
module DefinitionParams = Gprotocol.DefinitionParams
module TextDocumentPositionParams = Gprotocol.TextDocumentPositionParams
module Range = Gprotocol.Range
module DocumentLinkParams = Gprotocol.DocumentLinkParams
module DocumentLink = Gprotocol.DocumentLink
module ReferenceParams = Gprotocol.ReferenceParams
module Location = Gprotocol.Location
module DocumentHighlightParams = Gprotocol.DocumentHighlightParams
module DocumentHighlight = Gprotocol.DocumentHighlight

type _ t =
  | Shutdown : unit t
  | Initialize : InitializeParams.t -> InitializeResult.t t
  | TextDocumentHover : HoverParams.t -> Hover.t option t
  | TextDocumentDefinition : DefinitionParams.t -> Locations.t option t
  | TextDocumentDeclaration :
      TextDocumentPositionParams.t
      -> Locations.t option t
  | TextDocumentTypeDefinition : TypeDefinitionParams.t -> Locations.t option t
  | TextDocumentCompletion : Completion.params -> Completion.result t
  | TextDocumentCodeLens : CodeLensParams.t -> CodeLens.t list t
  | TextDocumentCodeLensResolve : CodeLens.t -> CodeLens.t t
  | TextDocumentPrepareRename : PrepareRenameParams.t -> Range.t option t
  | TextDocumentRename : RenameParams.t -> WorkspaceEdit.t t
  | TextDocumentLink : DocumentLinkParams.t -> DocumentLink.t list option t
  | TextDocumentLinkResolve : DocumentLink.t -> DocumentLink.t t
  | DocumentSymbol :
      DocumentSymbolParams.t
      -> [ `DocumentSymbol of DocumentSymbol.t list
         | `SymbolInformation of SymbolInformation.t list
         ]
         option
         t
  | WorkspaceSymbol :
      WorkspaceSymbolParams.t
      -> SymbolInformation.t list option t
  | DebugEcho : DebugEcho.params -> DebugEcho.result t
  | DebugTextDocumentGet :
      DebugTextDocumentGet.params
      -> DebugTextDocumentGet.result t
  | TextDocumentReferences : ReferenceParams.t -> Location.t list option t
  | TextDocumentHighlight :
      DocumentHighlightParams.t
      -> DocumentHighlight.t list option t
  | TextDocumentFoldingRange : FoldingRange.params -> FoldingRange.result t
  | SignatureHelp : TextDocumentPositionParams.t -> SignatureHelp.t t
  | CodeAction : CodeActionParams.t -> CodeActionResult.t t
  | CompletionItemResolve :
      Completion.completionItem
      -> Completion.completionItem t
  | WillSaveWaitUntilTextDocument :
      WillSaveTextDocumentParams.t
      -> TextEdit.t list option t
  | TextDocumentFormatting :
      DocumentFormattingParams.t
      -> TextDocumentFormatting.Result.t t
  | TextDocumentOnTypeFormatting :
      DocumentOnTypeFormattingParams.t
      -> TextDocumentOnTypeFormatting.Result.t t
  | TextDocumentColorPresentation :
      ColorPresentation.Params.t
      -> ColorPresentation.t list t
  | TextDocumentColor : DocumentColorParams.t -> ColorInformation.t list t
  | SelectionRange : SelectionRangeParams.t -> SelectionRange.t list t
  | ExecuteCommand : ExecuteCommandParams.t -> Json.t t
  | UnknownRequest : string * Json.t option -> unit t

let yojson_of_DocumentSymbol ds : Json.t =
  Json.Option.yojson_of_t
    (function
      | `DocumentSymbol ds -> Json.To.list DocumentSymbol.yojson_of_t ds
      | `SymbolInformation si -> Json.To.list SymbolInformation.yojson_of_t si)
    ds

let yojson_of_result (type a) (req : a t) (result : a) =
  match (req, result) with
  | Shutdown, () -> None
  | Initialize _, result -> Some (InitializeResult.yojson_of_t result)
  | TextDocumentDeclaration _, result ->
    Some (yojson_of_option Locations.yojson_of_t result)
  | TextDocumentHover _, result ->
    Some (Json.Option.yojson_of_t Hover.yojson_of_t result)
  | TextDocumentDefinition _, result ->
    Some (yojson_of_option Locations.yojson_of_t result)
  | TextDocumentTypeDefinition _, result ->
    Some (yojson_of_option Locations.yojson_of_t result)
  | TextDocumentCompletion _, result ->
    Some (Completion.yojson_of_result result)
  | TextDocumentCodeLens _, result ->
    Some (Json.To.list CodeLens.yojson_of_t result)
  | TextDocumentCodeLensResolve _, result -> Some (CodeLens.yojson_of_t result)
  | TextDocumentPrepareRename _, result ->
    Some (Json.Option.yojson_of_t Range.yojson_of_t result)
  | TextDocumentRename _, result -> Some (WorkspaceEdit.yojson_of_t result)
  | DocumentSymbol _, result -> Some (yojson_of_DocumentSymbol result)
  | DebugEcho _, result -> Some (DebugEcho.yojson_of_result result)
  | DebugTextDocumentGet _, result ->
    Some (DebugTextDocumentGet.yojson_of_result result)
  | TextDocumentReferences _, result ->
    Some (Json.Option.yojson_of_t (Json.To.list Location.yojson_of_t) result)
  | TextDocumentHighlight _, result ->
    Some
      (Json.Option.yojson_of_t
         (Json.To.list DocumentHighlight.yojson_of_t)
         result)
  | TextDocumentFoldingRange _, result ->
    Some (FoldingRange.yojson_of_result result)
  | SignatureHelp _, result -> Some (SignatureHelp.yojson_of_t result)
  | CodeAction _, result -> Some (CodeActionResult.yojson_of_t result)
  | CompletionItemResolve _, result ->
    Some (Completion.yojson_of_completionItem result)
  | WillSaveWaitUntilTextDocument _, result ->
    Some (Json.Option.yojson_of_t (Json.To.list TextEdit.yojson_of_t) result)
  | TextDocumentOnTypeFormatting _, result ->
    Some (TextDocumentOnTypeFormatting.Result.yojson_of_t result)
  | TextDocumentFormatting _, result ->
    Some (TextDocumentFormatting.Result.yojson_of_t result)
  | TextDocumentLink _, result ->
    Some
      (Json.Option.yojson_of_t (Json.To.list DocumentLink.yojson_of_t) result)
  | TextDocumentLinkResolve _, result -> Some (DocumentLink.yojson_of_t result)
  | WorkspaceSymbol _, result ->
    Some
      (Json.Option.yojson_of_t
         (Json.To.list SymbolInformation.yojson_of_t)
         result)
  | TextDocumentColorPresentation _, result ->
    Some (ColorPresentation.Result.yojson_of_t result)
  | TextDocumentColor _, result ->
    Some (Json.To.list ColorInformation.yojson_of_t result)
  | SelectionRange _, result ->
    Some (Json.yojson_of_list SelectionRange.yojson_of_t result)
  | ExecuteCommand _, result -> Some result
  | UnknownRequest _, _resp -> None

type packed = E : 'r t -> packed

let of_jsonrpc (r : Jsonrpc.Request.t) =
  let open Result.O in
  let parse f = Jsonrpc.Request.params r f in
  match r.method_ with
  | "initialize" ->
    parse InitializeParams.t_of_yojson >>| fun params -> E (Initialize params)
  | "shutdown" -> Ok (E Shutdown)
  | "textDocument/completion" ->
    parse Completion.params_of_yojson >>| fun params ->
    E (TextDocumentCompletion params)
  | "textDocument/documentSymbol" ->
    parse DocumentSymbolParams.t_of_yojson >>| fun params ->
    E (DocumentSymbol params)
  | "textDocument/hover" ->
    parse HoverParams.t_of_yojson >>| fun params -> E (TextDocumentHover params)
  | "textDocument/definition" ->
    parse DefinitionParams.t_of_yojson >>| fun params ->
    E (TextDocumentDefinition params)
  | "textDocument/typeDefinition" ->
    parse TypeDefinitionParams.t_of_yojson >>| fun params ->
    E (TextDocumentTypeDefinition params)
  | "textDocument/references" ->
    parse ReferenceParams.t_of_yojson >>| fun params ->
    E (TextDocumentReferences params)
  | "textDocument/codeLens" ->
    parse CodeLensParams.t_of_yojson >>| fun params ->
    E (TextDocumentCodeLens params)
  | "textDocument/rename" ->
    parse RenameParams.t_of_yojson >>| fun params ->
    E (TextDocumentRename params)
  | "textDocument/documentHighlight" ->
    parse DocumentHighlightParams.t_of_yojson >>| fun params ->
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
  | "textDocument/onTypeFormatting" ->
    parse DocumentOnTypeFormattingParams.t_of_yojson >>| fun params ->
    E (TextDocumentOnTypeFormatting params)
  | "textDocument/formatting" ->
    parse DocumentFormattingParams.t_of_yojson >>| fun params ->
    E (TextDocumentFormatting params)
  | "textDocument/documentLink" ->
    parse DocumentLinkParams.t_of_yojson >>| fun params ->
    E (TextDocumentLink params)
  | "textDocument/resolve" ->
    parse DocumentLink.t_of_yojson >>| fun params ->
    E (TextDocumentLinkResolve params)
  | "workspace/symbol" ->
    parse WorkspaceSymbolParams.t_of_yojson >>| fun params ->
    E (WorkspaceSymbol params)
  | "textDocument/colorPresentation" ->
    parse ColorPresentation.Params.t_of_yojson >>| fun params ->
    E (TextDocumentColorPresentation params)
  | "textDocument/documentColor" ->
    parse DocumentColorParams.t_of_yojson >>| fun params ->
    E (TextDocumentColor params)
  | "textDocument/declaration" ->
    parse TextDocumentPositionParams.t_of_yojson >>| fun params ->
    E (TextDocumentDeclaration params)
  | "textDocument/selectionRange" ->
    parse SelectionRangeParams.t_of_yojson >>| fun params ->
    E (SelectionRange params)
  | "workspace/executeCommand" ->
    parse ExecuteCommandParams.t_of_yojson >>| fun params ->
    E (ExecuteCommand params)
  | m -> Ok (E (UnknownRequest (m, r.params)))
