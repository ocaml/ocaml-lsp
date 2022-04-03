(* All modules from [Stdune] should be in the struct below. The modules are
   listed alphabetically. Try to keep the order. *)
include struct
  open Stdune
  module Array = Array
  module Code_error = Code_error
  module Comparable = Comparable
  module Exn_with_backtrace = Exn_with_backtrace
  module Fdecl = Fdecl
  module Fpath = Path
  module Int = Int
  module List = List
  module Map = Map
  module Monoid = Monoid
  module Option = Option
  module Ordering = Ordering
  module Pid = Pid
  module Poly = Poly
  module Result = Result

  module String = struct
    include String

    let findi =
      let rec loop s len ~f i =
        if i >= len then
          None
        else if f (String.unsafe_get s i) then
          Some i
        else
          loop s len ~f (i + 1)
      in
      fun ?from s ~f ->
        let len = String.length s in
        let from =
          match from with
          | None -> 0
          | Some i ->
            if i > len - 1 then
              Code_error.raise "findi: invalid from" []
            else
              i
        in
        loop s len ~f from

    let rfindi =
      let rec loop s ~f i =
        if i < 0 then
          None
        else if f (String.unsafe_get s i) then
          Some i
        else
          loop s ~f (i - 1)
      in
      fun ?from s ~f ->
        let from =
          let len = String.length s in
          match from with
          | None -> len - 1
          | Some i ->
            if i > len - 1 then
              Code_error.raise "rfindi: invalid from" []
            else
              i
        in
        loop s ~f from
  end

  module Table = Table
  module Tuple = Tuple
  module Unix_env = Env
  module Io = Io

  let sprintf = sprintf
end

(* All modules from [Lsp] should be in the struct below. The modules are listed
   alphabetically. Try to keep the order. *)
include struct
  open Lsp
  module Client_notification = Client_notification
  module Client_request = Client_request
  module Json = Import.Json
  module Server_request = Server_request
  module Text_document = Text_document
  module Uri = Uri
end

(* Misc modules *)
module Drpc = Dune_rpc.V1

(* OCaml frontend *)
module Ast_iterator = Ocaml_parsing.Ast_iterator
module Asttypes = Ocaml_parsing.Asttypes
module Cmt_format = Ocaml_typing.Cmt_format
module Ident = Ocaml_typing.Ident
module Loc = Ocaml_parsing.Location
module Longident = Ocaml_parsing.Longident
module Parsetree = Ocaml_parsing.Parsetree
module Path = Ocaml_typing.Path
module Typedtree = Ocaml_typing.Typedtree
module Types = Ocaml_typing.Types
module Warnings = Ocaml_utils.Warnings

(* All modules from [Lsp_fiber] should be in the struct below. The modules are
   listed alphabetically. Try to keep the order. *)
include struct
  open Lsp_fiber
  module Log = Import.Log
  module Reply = Rpc.Reply
  module Server = Server
  module Lazy_fiber = Lsp_fiber.Lazy_fiber
end

(* All modules from [Lsp.Types] should be in the struct below. The modules are
   listed alphabetically. Try to keep the order. *)
include struct
  open Lsp.Types

  module ClientCapabilities = struct
    include ClientCapabilities

    let markdown_support (client_capabilities : ClientCapabilities.t) ~field =
      match client_capabilities.textDocument with
      | None -> false
      | Some td -> (
        match field td with
        | None -> false
        | Some format ->
          let set = Option.value format ~default:[ MarkupKind.Markdown ] in
          List.mem set MarkupKind.Markdown ~equal:Poly.equal)
  end

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
  module DidChangeWorkspaceFoldersParams = DidChangeWorkspaceFoldersParams
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module DocumentFilter = DocumentFilter
  module DocumentHighlight = DocumentHighlight
  module DocumentHighlightKind = DocumentHighlightKind
  module DocumentHighlightParams = DocumentHighlightParams
  module DocumentSymbol = DocumentSymbol
  module DocumentUri = DocumentUri
  module ExecuteCommandOptions = ExecuteCommandOptions
  module ExecuteCommandParams = ExecuteCommandParams
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
  module Registration = Registration
  module RegistrationParams = RegistrationParams
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
  module TextDocumentClientCapabilities = TextDocumentClientCapabilities
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextDocumentEdit = TextDocumentEdit
  module TextDocumentIdentifier = TextDocumentIdentifier
  module TextDocumentItem = TextDocumentItem
  module TextDocumentRegistrationOptions = TextDocumentRegistrationOptions
  module TextDocumentSyncKind = TextDocumentSyncKind
  module TextDocumentSyncOptions = TextDocumentSyncOptions
  module TextDocumentSyncClientCapabilities = TextDocumentSyncClientCapabilities
  module TextEdit = TextEdit
  module TraceValue = TraceValue
  module VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
  module WorkDoneProgressBegin = WorkDoneProgressBegin
  module WorkDoneProgressCreateParams = WorkDoneProgressCreateParams
  module WorkDoneProgressEnd = WorkDoneProgressEnd
  module WorkDoneProgressReport = WorkDoneProgressReport
  module WorkspaceEdit = WorkspaceEdit
  module WorkspaceFolder = WorkspaceFolder
  module WorkspaceFoldersChangeEvent = WorkspaceFoldersChangeEvent
  module WorkspaceSymbolParams = WorkspaceSymbolParams
  module WorkspaceFoldersServerCapabilities = WorkspaceFoldersServerCapabilities
end

let task_if_running pool ~f =
  let open Fiber.O in
  let* running = Fiber.Pool.running pool in
  match running with
  | false -> Fiber.return ()
  | true -> Fiber.Pool.task pool ~f

let inside_test =
  match Sys.getenv_opt "OCAMLLSP_TEST" with
  | Some "true" -> true
  | None
  | Some "false" ->
    false
  | Some b ->
    Format.eprintf
      "invalid value %S for OCAMLLSP_TEST ignored. Only true or false are \
       allowed@."
      b;
    false
