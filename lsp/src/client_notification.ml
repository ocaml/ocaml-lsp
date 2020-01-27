open Import
open Protocol

type t =
  | TextDocumentDidOpen of DidOpen.params
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | DidSaveTextDocument of DidSaveTextDocumentParams.t
  | WillSaveTextDocument of WillSaveTextDocumentParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFolders.Params.t
  | ChangeConfiguration of DidChangeConfiguration.Params.t
  | Initialized
  | Exit
  | Unknown_notification of Jsonrpc.Request.t

let of_jsonrpc (r : Jsonrpc.Request.t) =
  let open Result.O in
  match r.method_ with
  | "textDocument/didOpen" ->
    Jsonrpc.Request.params r DidOpen.params_of_yojson >>| fun params ->
    TextDocumentDidOpen params
  | "textDocument/didChange" ->
    Jsonrpc.Request.params r DidChangeTextDocumentParams.t_of_yojson
    >>| fun params -> TextDocumentDidChange params
  | "exit" -> Ok Exit
  | "initialized" -> Ok Initialized
  | "workspace/didChangeWorkspaceFolders" ->
    Jsonrpc.Request.params r DidChangeWorkspaceFolders.Params.t_of_yojson
    >>| fun params -> ChangeWorkspaceFolders params
  | "workspace/didChangeConfiguration" ->
    Jsonrpc.Request.params r DidChangeConfiguration.Params.t_of_yojson
    >>| fun params -> ChangeConfiguration params
  | "textdocument/willSave" ->
    Jsonrpc.Request.params r WillSaveTextDocumentParams.t_of_yojson
    >>| fun params -> WillSaveTextDocument params
  | "textdocument/didSave" ->
    Jsonrpc.Request.params r DidSaveTextDocumentParams.t_of_yojson
    >>| fun params -> DidSaveTextDocument params
  | _ -> Ok (Unknown_notification r)
