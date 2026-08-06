(* The modules are listed alphabetically. Try to keep the order. *)

include struct
  open Base
  module Array = Array
  module Char = Char
  module Comparator = Comparator
  module Either = Either
  module Float = Float
  module Fn = Fn
  module Hashtbl = Hashtbl
  module Int = Int
  module List = List
  module Map = Map
  module Option = Option
  module Poly = Poly
  module Queue = Queue
  module Result = Result
  module Set = Set
  module Staged = Staged
  module String = String
end

let sprintf = Printf.sprintf

module Poly = struct
  include Base.Poly

  let hash = Base.Hashtbl.hash
end

module Exn_with_backtrace = Stdune.Exn_with_backtrace

module Id = struct
  module Make () = struct
    type t = int

    let next = ref 0

    let gen () =
      let id = !next in
      Int.incr next;
      id
    ;;

    let to_int t = t
    let compare x y = Ordering.of_int (Int.compare x y)
  end
end

module Monoid = Stdune.Monoid

module Result = struct
  module O = struct
    let ( let+ ) x f = Stdlib.Result.map f x
    let ( let* ) x f = Stdlib.Result.bind x f
  end

  include Base.Result
end

module Option = struct
  module O = struct
    let ( let+ ) x f = Stdlib.Option.map f x
    let ( let* ) x f = Stdlib.Option.bind x f
  end

  include Base.Option
end

module Env_vars = struct
  open Option.O

  let _TEST () : bool option =
    let+ v = Sys.getenv_opt "OCAMLLSP_TEST" in
    match v with
    | "true" -> true
    | "false" -> false
    | unexpected_val ->
      Format.eprintf
        "invalid value %S for OCAMLLSP_TEST ignored. Only true or false are allowed@."
        unexpected_val;
      false
  ;;

  let _IS_HOVER_EXTENDED () : bool option =
    let* v = Sys.getenv_opt "OCAMLLSP_HOVER_IS_EXTENDED" in
    match v with
    | "true" | "1" -> Some true
    | _ -> Some false
  ;;
end

(* All modules from [Lsp] should be in the struct below. The modules are listed
   alphabetically. Try to keep the order. *)
include struct
  open Lsp
  module Deprecation = Deprecation
  module Client_notification = Client_notification
  module Client_request = Client_request
  module Server_request = Server_request
  module Text_document = Text_document
  module Workspaces = Workspaces

  module Uri = struct
    module Uri = struct
      include Uri

      let to_dyn t = Dyn.string (to_string t)
      let sexp_of_t t = Sexplib0.Sexp_conv.sexp_of_string (to_string t)
    end

    include Uri
    include Comparator.Make (Uri)
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
  module Map = Stdlib.MoreLabels.Map.Make (T)
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
  module Data_types = Data_types
end

include struct
  open Merlin_kernel
  module Mconfig = Mconfig
  module Mconfig_dot = Mconfig_dot
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
  module Fdecl = Private.Fdecl
  module Reply = Rpc.Reply
  module Server = Server
  module Lazy_fiber = Lsp_fiber.Lazy_fiber
  module Json = Json
end

(* All modules from [Lsp.Types] should be in the struct below. The modules are
   listed alphabetically. Try to keep the order. *)
include struct
  open Lsp.Types
  module Capabilities = Lsp.Capabilities
  module ClientCapabilities = ClientCapabilities
  module CodeAction = CodeAction
  module CodeActionDisabled = CodeActionDisabled
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
  module CompletionItemTag = CompletionItemTag
  module CompletionItemTagOptions = CompletionItemTagOptions
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
  module Experimental = Lsp.Experimental
  module FoldingRange = FoldingRange
  module FoldingRangeKind = FoldingRangeKind
  module FoldingRangeParams = FoldingRangeParams
  module Hover = Hover
  module HoverParams = HoverParams
  module InlayHint = InlayHint
  module InlayHintKind = InlayHintKind
  module InlayHintParams = InlayHintParams
  module InitializeParams = InitializeParams
  module InitializeResult = InitializeResult
  module LanguageKind = LanguageKind
  module Location = Location
  module LogMessageParams = LogMessageParams
  module LogTraceParams = LogTraceParams
  module MarkupContent = MarkupContent
  module MarkupKind = MarkupKind
  module MessageType = MessageType
  module OptionalVersionedTextDocumentIdentifier = OptionalVersionedTextDocumentIdentifier
  module ParameterInformation = ParameterInformation
  module PositionEncodingKind = PositionEncodingKind
  module PrepareRenameParams = PrepareRenameParams
  module ProgressParams = ProgressParams
  module ProgressToken = ProgressToken
  module PublishDiagnosticsParams = PublishDiagnosticsParams
  module PublishDiagnosticsClientCapabilities = PublishDiagnosticsClientCapabilities
  module ReferenceParams = ReferenceParams
  module Registration = Registration
  module RegistrationParams = RegistrationParams
  module RenameOptions = RenameOptions
  module RenameParams = RenameParams
  module SaveOptions = SaveOptions
  module SelectionRange = SelectionRange
  module SelectionRangeParams = SelectionRangeParams
  module SemanticTokens = SemanticTokens
  module SemanticTokensClientCapabilities = SemanticTokensClientCapabilities
  module SemanticTokensEdit = SemanticTokensEdit
  module SemanticTokensFullDelta = SemanticTokensFullDelta
  module SemanticTokensLegend = SemanticTokensLegend
  module SemanticTokensDelta = SemanticTokensDelta
  module SemanticTokensDeltaParams = SemanticTokensDeltaParams
  module SemanticTokenModifiers = SemanticTokenModifiers
  module SemanticTokensOptions = SemanticTokensOptions
  module SemanticTokensParams = SemanticTokensParams
  module SemanticTokenTypes = SemanticTokenTypes
  module ServerCapabilities = ServerCapabilities
  module ServerInfo = ServerInfo
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
  module SymbolTag = SymbolTag
  module TextDocumentClientCapabilities = TextDocumentClientCapabilities
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextDocumentContentChangePartial = TextDocumentContentChangePartial
  module TextDocumentContentChangeWholeDocument = TextDocumentContentChangeWholeDocument
  module TextDocumentEdit = TextDocumentEdit
  module TextDocumentFilter = TextDocumentFilter
  module TextDocumentIdentifier = TextDocumentIdentifier
  module TextDocumentItem = TextDocumentItem
  module TextDocumentRegistrationOptions = TextDocumentRegistrationOptions
  module TextDocumentSyncKind = TextDocumentSyncKind
  module TextDocumentSyncOptions = TextDocumentSyncOptions
  module TextDocumentSyncClientCapabilities = TextDocumentSyncClientCapabilities
  module TextEdit = TextEdit
  module TraceValue = TraceValue
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
  module WorkspaceOptions = WorkspaceOptions
end

let task_if_running pool ~f =
  let open Fiber.O in
  let* running = Fiber.Pool.running pool in
  match running with
  | false -> Fiber.return ()
  | true -> Fiber.Pool.task pool ~f
;;

let inside_test = Env_vars._TEST () |> Option.value ~default:false
