(* All modules from [Stdune] should be in the struct below. The modules are
   listed alphabetically. Try to keep the order. *)
include struct
  open Stdune

  module Array = struct
    include Array

    let common_prefix_len ~equal (a : 'a array) (b : 'a array) : int =
      let i = ref 0 in
      let min_len = min (Array.length a) (Array.length b) in
      while !i < min_len && equal (Array.get a !i) (Array.get b !i) do
        incr i
      done;
      !i
  end

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
  module Pid = Pid
  module Poly = Poly
  module Result = Result
  module Queue = Queue

  module String = struct
    include String

    (**Filters a string keeping any chars for which f returns true and
       discarding those for which it returns false*)
    let filter f s =
      let buf = Buffer.create (String.length s) in
      iter ~f:(fun c -> if f c then Buffer.add_char buf c) s;
      Buffer.contents buf

    let findi =
      let rec loop s len ~f i =
        if i >= len then None
        else if f (String.unsafe_get s i) then Some i
        else loop s len ~f (i + 1)
      in
      fun ?from s ~f ->
        let len = String.length s in
        let from =
          match from with
          | None -> 0
          | Some i ->
            if i > len - 1 then Code_error.raise "findi: invalid from" [] else i
        in
        loop s len ~f from

    let rfindi =
      let rec loop s ~f i =
        if i < 0 then None
        else if f (String.unsafe_get s i) then Some i
        else loop s ~f (i - 1)
      in
      fun ?from s ~f ->
        let from =
          let len = String.length s in
          match from with
          | None -> len - 1
          | Some i ->
            if i > len - 1 then Code_error.raise "rfindi: invalid from" []
            else i
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
  module Server_request = Server_request
  module Text_document = Text_document

  module Uri = struct
    include Uri

    let to_dyn t = Dyn.string (to_string t)
  end
end

(* Misc modules *)
module Drpc = Dune_rpc.V1

(* OCaml frontend *)
module Ast_iterator = Ocaml_parsing.Ast_iterator
module Asttypes = Ocaml_parsing.Asttypes
module Cmt_format = Ocaml_typing.Cmt_format
module Ident = Ocaml_typing.Ident
module Env = Ocaml_typing.Env
module Merlin_parsing = Ocaml_parsing

module Loc = struct
  module T = struct
    include Ocaml_parsing.Location
    include Ocaml_parsing.Location_aux
  end

  include T

  module Map = Map.Make (struct
    include T

    let compare x x' = Ordering.of_int (compare x x')

    let position_to_dyn (pos : Lexing.position) =
      Dyn.Record
        [ ("pos_fname", Dyn.String pos.pos_fname)
        ; ("pos_lnum", Dyn.Int pos.pos_lnum)
        ; ("pos_bol", Dyn.Int pos.pos_bol)
        ; ("pos_cnum", Dyn.Int pos.pos_cnum)
        ]

    let to_dyn loc =
      Dyn.Record
        [ ("loc_start", position_to_dyn loc.loc_start)
        ; ("loc_end", position_to_dyn loc.loc_end)
        ; ("loc_ghost", Dyn.Bool loc.loc_ghost)
        ]
  end)
end

include struct
  open Ocaml_parsing
  module Longident = Longident
  module Parsetree = Parsetree
  module Pprintast = Pprintast
end

include struct
  open Ocaml_typing
  module Path = Path
  module Typedtree = Typedtree
  module Types = Types
end

include struct
  open Merlin_kernel
  module Mconfig = Mconfig
  module Msource = Msource
  module Mbrowse = Mbrowse
  module Mpipeline = Mpipeline
  module Mreader = Mreader
  module Mtyper = Mtyper
end

module Warnings = Ocaml_utils.Warnings
module Browse_raw = Merlin_specific.Browse_raw
module Format = Merlin_utils.Std.Format

(* All modules from [Lsp_fiber] should be in the struct below. The modules are
   listed alphabetically. Try to keep the order. *)
include struct
  open Lsp_fiber
  module Log = Private.Log
  module Reply = Rpc.Reply
  module Server = Server
  module Lazy_fiber = Lsp_fiber.Lazy_fiber
  module Json = Json
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
  module CodeActionRegistrationOptions = CodeActionRegistrationOptions
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
  module CreateFile = CreateFile
  module Diagnostic = Diagnostic
  module DiagnosticRelatedInformation = DiagnosticRelatedInformation
  module DiagnosticSeverity = DiagnosticSeverity
  module DiagnosticTag = DiagnosticTag
  module DidChangeConfigurationParams = DidChangeConfigurationParams
  module DidChangeWorkspaceFoldersParams = DidChangeWorkspaceFoldersParams
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module Diff = Lsp.Diff
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
  module InlayHint = InlayHint
  module InlayHintKind = InlayHintKind
  module InlayHintParams = InlayHintParams
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
  module PositionEncodingKind = PositionEncodingKind
  module ProgressParams = ProgressParams
  module ProgressToken = ProgressToken
  module PublishDiagnosticsParams = PublishDiagnosticsParams
  module PublishDiagnosticsClientCapabilities =
    PublishDiagnosticsClientCapabilities
  module ReferenceParams = ReferenceParams
  module Registration = Registration
  module RegistrationParams = RegistrationParams
  module RenameOptions = RenameOptions
  module RenameParams = RenameParams
  module SaveOptions = SaveOptions
  module SelectionRange = SelectionRange
  module SelectionRangeParams = SelectionRangeParams
  module SemanticTokens = SemanticTokens
  module SemanticTokensEdit = SemanticTokensEdit
  module SemanticTokensLegend = SemanticTokensLegend
  module SemanticTokensDelta = SemanticTokensDelta
  module SemanticTokensDeltaParams = SemanticTokensDeltaParams
  module SemanticTokenModifiers = SemanticTokenModifiers
  module SemanticTokensOptions = SemanticTokensOptions
  module SemanticTokensParams = SemanticTokensParams
  module SemanticTokenTypes = SemanticTokenTypes
  module ServerCapabilities = ServerCapabilities
  module Server_notification = Lsp.Server_notification
  module SetTraceParams = SetTraceParams
  module ShowDocumentClientCapabilities = ShowDocumentClientCapabilities
  module ShowDocumentParams = ShowDocumentParams
  module ShowDocumentResult = ShowDocumentResult
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
  module TextDocumentFilter = TextDocumentFilter
  module TextDocumentIdentifier = TextDocumentIdentifier
  module TextDocumentItem = TextDocumentItem
  module TextDocumentRegistrationOptions = TextDocumentRegistrationOptions
  module TextDocumentSyncKind = TextDocumentSyncKind
  module TextDocumentSyncOptions = TextDocumentSyncOptions
  module TextDocumentSyncClientCapabilities = TextDocumentSyncClientCapabilities
  module TextEdit = TextEdit

  (** deprecated *)
  module TraceValue = TraceValues

  module TraceValues = TraceValues
  module Unregistration = Unregistration
  module UnregistrationParams = UnregistrationParams
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

let inside_test = Env_vars._TEST () |> Option.value ~default:false
