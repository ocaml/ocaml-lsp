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
  | CancelRequest of Jsonrpc.Id.t
  | WorkDoneProgressCancel of WorkDoneProgressCancelParams.t
  | SetTrace of SetTraceParams.t
  | Unknown_notification of Jsonrpc.Message.notification

let method_ = function
  | TextDocumentDidOpen _ -> "textDocument/didOpen"
  | TextDocumentDidChange _ -> "textDocument/didChange"
  | TextDocumentDidClose _ -> "textDocument/didClose"
  | Exit -> "exit"
  | Initialized -> "initialized"
  | ChangeWorkspaceFolders _ -> "workspace/didChangeWorkspaceFolders"
  | ChangeConfiguration _ -> "workspace/didChangeConfiguration"
  | WillSaveTextDocument _ -> "textDocument/willSave"
  | DidSaveTextDocument _ -> "textDocument/didSave"
  | SetTrace _ -> "$/setTrace"
  | CancelRequest _ -> Cancel_request.meth_
  | WorkDoneProgressCancel _ -> "window/workDoneProgress/cancel"
  | Unknown_notification _ -> assert false

let yojson_of_t = function
  | TextDocumentDidOpen params ->
    Some (DidOpenTextDocumentParams.yojson_of_t params)
  | TextDocumentDidChange params ->
    Some (DidChangeTextDocumentParams.yojson_of_t params)
  | TextDocumentDidClose params ->
    Some (DidCloseTextDocumentParams.yojson_of_t params)
  | Exit -> None
  | Initialized -> None
  | ChangeWorkspaceFolders params ->
    Some (DidChangeWorkspaceFoldersParams.yojson_of_t params)
  | ChangeConfiguration params ->
    Some (DidChangeConfigurationParams.yojson_of_t params)
  | WillSaveTextDocument params ->
    Some (WillSaveTextDocumentParams.yojson_of_t params)
  | DidSaveTextDocument params ->
    Some (DidSaveTextDocumentParams.yojson_of_t params)
  | CancelRequest params -> Some (Cancel_request.yojson_of_t params)
  | WorkDoneProgressCancel params ->
    Some (WorkDoneProgressCancelParams.yojson_of_t params)
  | SetTrace params -> Some (SetTraceParams.yojson_of_t params)
  | Unknown_notification _ -> assert false

let of_jsonrpc (r : Jsonrpc.Message.notification) =
  let open Result.O in
  match r.method_ with
  | "textDocument/didOpen" ->
    let+ params = Json.message_params r DidOpenTextDocumentParams.t_of_yojson in
    TextDocumentDidOpen params
  | "textDocument/didChange" ->
    let+ params =
      Json.message_params r DidChangeTextDocumentParams.t_of_yojson
    in
    TextDocumentDidChange params
  | "textDocument/didClose" ->
    let+ params =
      Json.message_params r DidCloseTextDocumentParams.t_of_yojson
    in
    TextDocumentDidClose params
  | "exit" -> Ok Exit
  | "initialized" -> Ok Initialized
  | "workspace/didChangeWorkspaceFolders" ->
    let+ params =
      Json.message_params r DidChangeWorkspaceFoldersParams.t_of_yojson
    in
    ChangeWorkspaceFolders params
  | "workspace/didChangeConfiguration" ->
    let+ params =
      Json.message_params r DidChangeConfigurationParams.t_of_yojson
    in
    ChangeConfiguration params
  | "textDocument/willSave" ->
    let+ params =
      Json.message_params r WillSaveTextDocumentParams.t_of_yojson
    in
    WillSaveTextDocument params
  | "textDocument/didSave" ->
    let+ params = Json.message_params r DidSaveTextDocumentParams.t_of_yojson in
    DidSaveTextDocument params
  | m when m = Cancel_request.meth_ ->
    let+ params = Json.message_params r Cancel_request.t_of_yojson in
    CancelRequest params
  | "window/workDoneProgress/cancel" ->
    let+ params =
      Json.message_params r WorkDoneProgressCancelParams.t_of_yojson
    in
    WorkDoneProgressCancel params
  | "$/setTrace" ->
    let+ params = Json.message_params r SetTraceParams.t_of_yojson in
    SetTrace params
  | _ -> Ok (Unknown_notification r)

let to_jsonrpc t =
  let method_ = method_ t in
  let params = yojson_of_t t |> Option.map Jsonrpc.Message.Structured.of_json in
  { Jsonrpc.Message.id = (); params; method_ }
