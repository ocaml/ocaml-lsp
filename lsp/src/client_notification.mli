open Import
open Protocol

type t =
  | TextDocumentDidOpen of DidOpen.params
  | TextDocumentDidClose of DidCloseTextDocumentParams.t
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | DidSaveTextDocument of DidSaveTextDocumentParams.t
  | WillSaveTextDocument of WillSaveTextDocumentParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFolders.Params.t
  | ChangeConfiguration of DidChangeConfiguration.Params.t
  | Initialized
  | Exit
  | Unknown_notification of Jsonrpc.Request.t

val of_jsonrpc : Jsonrpc.Request.t -> (t, string) Result.t
