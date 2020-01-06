open Import
open Protocol

type t =
  | TextDocumentDidOpen of DidOpen.params
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | Initialized
  | Exit
  | Unknown_notification of Jsonrpc.Request.t

val of_jsonrpc : Jsonrpc.Request.t -> (t, string) Result.t
