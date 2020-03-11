open! Import
open Protocol
module InitializeParams = Gprotocol.InitializeParams
module InitializeResult = Gprotocol.InitializeResult
module CodeActionParams = Gprotocol.CodeActionParams
module CodeActionResult = Gprotocol.CodeActionResult
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
      -> TextEdit.t list option t
  | TextDocumentFormatting : DocumentFormattingParams.t -> TextEdit.t list t
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

val yojson_of_result : 'a t -> 'a -> Json.t option

type packed = E : 'r t -> packed

val of_jsonrpc : Jsonrpc.Request.t -> (packed, string) Result.t
