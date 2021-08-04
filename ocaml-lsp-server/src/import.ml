module Fpath = Stdune.Path
module Ordering = Stdune.Ordering
module Json = Lsp.Import.Json
module Unix_env = Stdune.Env
module Code_error = Stdune.Code_error
module Int = Stdune.Int
module Dyn = Stdune.Dyn
module Option = Stdune.Option
module Table = Stdune.Table
module String = Stdune.String
module List = Stdune.List
module Array = Stdune.Array
module Result = Stdune.Result
module Poly = Stdune.Poly
module Fdecl = Stdune.Fdecl
module Comparable = Stdune.Comparable
module Exn_with_backtrace = Stdune.Exn_with_backtrace
module Loc = Location
module Scheduler = Fiber_unix.Scheduler
module Server = Lsp_fiber.Server
module Client_request = Lsp.Client_request
module Server_request = Lsp.Server_request
module Client_notification = Lsp.Client_notification
module Text_document = Lsp.Text_document

include struct
  open Lsp.Types
  module CompletionItem = CompletionItem
  module CompletionItemKind = CompletionItemKind
  module CompletionList = CompletionList
  module CompletionOptions = CompletionOptions
  module CompletionParams = CompletionParams
  module SymbolKind = SymbolKind
  module InitializeResult = InitializeResult
  module InitializeParams = InitializeParams
  module SignatureHelpOptions = SignatureHelpOptions
  module CodeActionOptions = CodeActionOptions
  module CodeLensOptions = CodeLensOptions
  module TextDocumentSyncOptions = TextDocumentSyncOptions
  module TextDocumentSyncKind = TextDocumentSyncKind
  module ServerCapabilities = ServerCapabilities
  module Diagnostic = Diagnostic
  module DiagnosticRelatedInformation = DiagnosticRelatedInformation
  module PublishDiagnosticsParams = PublishDiagnosticsParams
  module MessageType = MessageType
  module WorkspaceEdit = WorkspaceEdit
  module ProgressToken = ProgressToken
  module WorkDoneProgressCreateParams = WorkDoneProgressCreateParams
  module WorkDoneProgressBegin = WorkDoneProgressBegin
  module WorkDoneProgressReport = WorkDoneProgressReport
  module WorkDoneProgressEnd = WorkDoneProgressEnd
  module ProgressParams = ProgressParams
  module ExecuteCommandOptions = ExecuteCommandOptions
  module TextEdit = TextEdit
  module CodeActionKind = CodeActionKind
  module ShowMessageParams = ShowMessageParams
  module ClientCapabilities = ClientCapabilities
  module DiagnosticSeverity = DiagnosticSeverity
  module ParameterInformation = ParameterInformation
  module SignatureInformation = SignatureInformation
  module SignatureHelpParams = SignatureHelpParams
  module SignatureHelp = SignatureHelp
  module CodeActionParams = CodeActionParams
  module CodeAction = CodeAction
  module CodeActionResult = CodeActionResult
  module MarkupContent = MarkupContent
  module MarkupKind = MarkupKind
  module Hover = Hover
  module Location = Location
  module Command = Command
  module CodeLens = CodeLens
  module DocumentHighlight = DocumentHighlight
  module DocumentHighlightParams = DocumentHighlightParams
  module DocumentHighlightKind = DocumentHighlightKind
  module DocumentSymbol = DocumentSymbol
  module DocumentUri = DocumentUri
  module SymbolInformation = SymbolInformation
  module VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
  module OptionalVersionedTextDocumentIdentifier =
    OptionalVersionedTextDocumentIdentifier
  module TextDocumentEdit = TextDocumentEdit
  module FoldingRange = FoldingRange
  module SelectionRange = SelectionRange
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextDocumentIdentifier = TextDocumentIdentifier
  module Server_notification = Lsp.Server_notification
  module HoverParams = HoverParams
  module SelectionRangeParams = SelectionRangeParams
  module RenameParams = RenameParams
  module CodeLensParams = CodeLensParams
  module FoldingRangeParams = FoldingRangeParams
  module ReferenceParams = ReferenceParams
  module DidChangeConfigurationParams = DidChangeConfigurationParams
  module ConfigurationParams = ConfigurationParams
  module RenameOptions = RenameOptions
  module WorkspaceFolder = WorkspaceFolder
  module WorkspaceSymbolParams = WorkspaceSymbolParams
  module TraceValue = TraceValue
  module SetTraceParams = SetTraceParams
  module LogMessageParams = LogMessageParams
end

module Uri = Lsp.Uri
module Io = Stdune.Io
module Reply = Lsp_fiber.Rpc.Reply
module Log = Lsp_fiber.Import.Log
module Drpc = Dune_rpc.V1
module Pid = Stdune.Pid

let sprintf = Stdune.sprintf
