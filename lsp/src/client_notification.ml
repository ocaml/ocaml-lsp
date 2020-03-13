open Import
open Types

type t =
  | TextDocumentDidOpen of DidOpenTextDocumentParams.t
  | TextDocumentDidClose of DidCloseTextDocumentParams.t
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | DidSaveTextDocument of DidSaveTextDocumentParams.t
  | WillSaveTextDocument of WillSaveTextDocumentParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFoldersParams.t
  | ChangeConfiguration of DidChangeConfigurationParams.t
  | Initialized
  | Exit
  | Unknown_notification of Jsonrpc.Request.t

let of_jsonrpc (r : Jsonrpc.Request.t) =
  let open Result.O in
  match r.method_ with
  | "textDocument/didOpen" ->
    Jsonrpc.Request.params r DidOpenTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidOpen params
  | "textDocument/didChange" ->
    Jsonrpc.Request.params r DidChangeTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidChange params
  | "textDocument/didClose" ->
    Jsonrpc.Request.params r DidCloseTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidClose params
  | "exit" -> Ok Exit
  | "initialized" -> Ok Initialized
  | "workspace/didChangeWorkspaceFolders" ->
    Jsonrpc.Request.params r DidChangeWorkspaceFoldersParams.t_of_yojson
    >>| fun params -> ChangeWorkspaceFolders params
  | "workspace/didChangeConfiguration" ->
    Jsonrpc.Request.params r DidChangeConfigurationParams.t_of_yojson
    >>| fun params -> ChangeConfiguration params
  | "textDocument/willSave" ->
    Jsonrpc.Request.params r WillSaveTextDocumentParams.t_of_yojson
    >>| fun params -> WillSaveTextDocument params
  | "textDocument/didSave" ->
    Jsonrpc.Request.params r DidSaveTextDocumentParams.t_of_yojson
    >>| fun params -> DidSaveTextDocument params
  | _ -> Ok (Unknown_notification r)
