(* All modules from [Stdune] should be in the struct below. *)
include struct
  open Stdune
  module Array = Array
  module Code_error = Code_error
  module Comparable = Comparable
  module Dyn = Dyn
  module Exn_with_backtrace = Exn_with_backtrace
  module Fdecl = Fdecl
  module Fpath = Path
  module Int = Int
  module List = List
  module Option = Option
  module Ordering = Ordering
  module Pid = Pid
  module Poly = Poly
  module Result = Result
  module String = String
  module Table = Table
  module Unix_env = Env
  module Io = Io

  let sprintf = sprintf
end

module Json = Lsp.Import.Json
module Loc = Location
module Scheduler = Fiber_unix.Scheduler
module Server = Lsp_fiber.Server
module Client_request = Lsp.Client_request
module Server_request = Lsp.Server_request
module Client_notification = Lsp.Client_notification
module Text_document = Lsp.Text_document

(* All modules from [Lsp.Types] should be in the struct below. The modules are
   listed alphabetically. Try to keep the order. *)
include struct
  open Lsp.Types
  module ClientCapabilities = ClientCapabilities
  module CodeAction = CodeAction
  module CodeActionKind = CodeActionKind
  module CodeActionOptions = CodeActionOptions
  module CodeActionParams = CodeActionParams
  module CodeActionResult = CodeActionResult
  module CodeLens = CodeLens
  module CodeLensOptions = CodeLensOptions
  module CodeLensParams = CodeLensParams
  module Command = Command
  module CompletionItem = CompletionItem
  module CompletionItemKind = CompletionItemKind
  module CompletionList = CompletionList
  module CompletionOptions = CompletionOptions
  module CompletionParams = CompletionParams
  module ConfigurationParams = ConfigurationParams
  module Diagnostic = Diagnostic
  module DiagnosticRelatedInformation = DiagnosticRelatedInformation
  module DiagnosticSeverity = DiagnosticSeverity
  module DidChangeConfigurationParams = DidChangeConfigurationParams
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module DocumentHighlight = DocumentHighlight
  module DocumentHighlightKind = DocumentHighlightKind
  module DocumentHighlightParams = DocumentHighlightParams
  module DocumentSymbol = DocumentSymbol
  module DocumentUri = DocumentUri
  module ExecuteCommandOptions = ExecuteCommandOptions
  module FoldingRange = FoldingRange
  module FoldingRangeParams = FoldingRangeParams
  module Hover = Hover
  module HoverParams = HoverParams
  module InitializeParams = InitializeParams
  module InitializeResult = InitializeResult
  module Location = Location
  module LogMessageParams = LogMessageParams
  module MarkupContent = MarkupContent
  module MarkupKind = MarkupKind
  module MessageType = MessageType
  module OptionalVersionedTextDocumentIdentifier =
    OptionalVersionedTextDocumentIdentifier
  module ParameterInformation = ParameterInformation
  module ProgressParams = ProgressParams
  module ProgressToken = ProgressToken
  module PublishDiagnosticsParams = PublishDiagnosticsParams
  module ReferenceParams = ReferenceParams
  module RenameOptions = RenameOptions
  module RenameParams = RenameParams
  module SelectionRange = SelectionRange
  module SelectionRangeParams = SelectionRangeParams
  module ServerCapabilities = ServerCapabilities
  module Server_notification = Lsp.Server_notification
  module SetTraceParams = SetTraceParams
  module ShowMessageParams = ShowMessageParams
  module SignatureHelp = SignatureHelp
  module SignatureHelpOptions = SignatureHelpOptions
  module SignatureHelpParams = SignatureHelpParams
  module SignatureInformation = SignatureInformation
  module SymbolInformation = SymbolInformation
  module SymbolKind = SymbolKind
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextDocumentEdit = TextDocumentEdit
  module TextDocumentIdentifier = TextDocumentIdentifier
  module TextDocumentSyncKind = TextDocumentSyncKind
  module TextDocumentSyncOptions = TextDocumentSyncOptions
  module TextEdit = TextEdit
  module TraceValue = TraceValue
  module VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
  module WorkDoneProgressBegin = WorkDoneProgressBegin
  module WorkDoneProgressCreateParams = WorkDoneProgressCreateParams
  module WorkDoneProgressEnd = WorkDoneProgressEnd
  module WorkDoneProgressReport = WorkDoneProgressReport
  module WorkspaceEdit = WorkspaceEdit
  module WorkspaceFolder = WorkspaceFolder
  module WorkspaceSymbolParams = WorkspaceSymbolParams
end

module Uri = Lsp.Uri
module Reply = Lsp_fiber.Rpc.Reply
module Log = Lsp_fiber.Import.Log
module Drpc = Dune_rpc.V1
