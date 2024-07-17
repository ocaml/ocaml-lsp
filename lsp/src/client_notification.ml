open Import
open Types

type t =
  | TextDocumentDidOpen of DidOpenTextDocumentParams.t
  | TextDocumentDidClose of DidCloseTextDocumentParams.t
  | TextDocumentDidChange of DidChangeTextDocumentParams.t
  | DidSaveTextDocument of DidSaveTextDocumentParams.t
  | WillSaveTextDocument of WillSaveTextDocumentParams.t
  | DidChangeWatchedFiles of DidChangeWatchedFilesParams.t
  | DidCreateFiles of CreateFilesParams.t
  | DidDeleteFiles of DeleteFilesParams.t
  | DidRenameFiles of RenameFilesParams.t
  | ChangeWorkspaceFolders of DidChangeWorkspaceFoldersParams.t
  | ChangeConfiguration of DidChangeConfigurationParams.t
  | Initialized
  | Exit
  | CancelRequest of Jsonrpc.Id.t
  | WorkDoneProgressCancel of WorkDoneProgressCancelParams.t
  | SetTrace of SetTraceParams.t
  | WorkDoneProgress of Progress.t ProgressParams.t
  | NotebookDocumentDidOpen of DidOpenNotebookDocumentParams.t
  | NotebookDocumentDidChange of DidChangeNotebookDocumentParams.t
  | NotebookDocumentDidSave of DidSaveNotebookDocumentParams.t
  | NotebookDocumentDidClose of DidCloseNotebookDocumentParams.t
  | UnknownNotification of Jsonrpc.Notification.t

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
  | DidChangeWatchedFiles _ -> "workspace/didChangeWatchedFiles"
  | DidCreateFiles _ -> "workspace/didCreateFiles"
  | DidDeleteFiles _ -> "workspace/didDeleteFiles"
  | DidRenameFiles _ -> "workspace/didRenameFiles"
  | SetTrace _ -> "$/setTrace"
  | CancelRequest _ -> Cancel_request.meth_
  | WorkDoneProgressCancel _ -> "window/workDoneProgress/cancel"
  | WorkDoneProgress _ -> Progress.method_
  | NotebookDocumentDidOpen _ -> "notebookDocument/didOpen"
  | NotebookDocumentDidChange _ -> "notebookDocument/didChange"
  | NotebookDocumentDidSave _ -> "notebookDocument/didSave"
  | NotebookDocumentDidClose _ -> "notebookDocument/didClose"
  | UnknownNotification n -> n.method_
;;

let yojson_of_t = function
  | TextDocumentDidOpen params -> Some (DidOpenTextDocumentParams.yojson_of_t params)
  | TextDocumentDidChange params -> Some (DidChangeTextDocumentParams.yojson_of_t params)
  | TextDocumentDidClose params -> Some (DidCloseTextDocumentParams.yojson_of_t params)
  | Exit -> None
  | Initialized -> None
  | ChangeWorkspaceFolders params ->
    Some (DidChangeWorkspaceFoldersParams.yojson_of_t params)
  | ChangeConfiguration params -> Some (DidChangeConfigurationParams.yojson_of_t params)
  | WillSaveTextDocument params -> Some (WillSaveTextDocumentParams.yojson_of_t params)
  | DidSaveTextDocument params -> Some (DidSaveTextDocumentParams.yojson_of_t params)
  | DidChangeWatchedFiles params -> Some (DidChangeWatchedFilesParams.yojson_of_t params)
  | DidCreateFiles params -> Some (CreateFilesParams.yojson_of_t params)
  | DidDeleteFiles params -> Some (DeleteFilesParams.yojson_of_t params)
  | DidRenameFiles params -> Some (RenameFilesParams.yojson_of_t params)
  | CancelRequest params -> Some (Cancel_request.yojson_of_t params)
  | WorkDoneProgressCancel params ->
    Some (WorkDoneProgressCancelParams.yojson_of_t params)
  | SetTrace params -> Some (SetTraceParams.yojson_of_t params)
  | WorkDoneProgress params ->
    Some ((ProgressParams.yojson_of_t Progress.yojson_of_t) params)
  | NotebookDocumentDidOpen params ->
    Some (DidOpenNotebookDocumentParams.yojson_of_t params)
  | NotebookDocumentDidClose params ->
    Some (DidCloseNotebookDocumentParams.yojson_of_t params)
  | NotebookDocumentDidChange params ->
    Some (DidChangeNotebookDocumentParams.yojson_of_t params)
  | NotebookDocumentDidSave params ->
    Some (DidSaveNotebookDocumentParams.yojson_of_t params)
  | UnknownNotification n -> (n.params :> Json.t option)
;;

let of_jsonrpc (r : Jsonrpc.Notification.t) =
  let open Result.O in
  let params = r.params in
  match r.method_ with
  | "textDocument/didOpen" ->
    let+ params = Json.message_params params DidOpenTextDocumentParams.t_of_yojson in
    TextDocumentDidOpen params
  | "textDocument/didChange" ->
    let+ params = Json.message_params params DidChangeTextDocumentParams.t_of_yojson in
    TextDocumentDidChange params
  | "textDocument/didClose" ->
    let+ params = Json.message_params params DidCloseTextDocumentParams.t_of_yojson in
    TextDocumentDidClose params
  | "exit" -> Ok Exit
  | "initialized" -> Ok Initialized
  | "workspace/didChangeWorkspaceFolders" ->
    let+ params =
      Json.message_params params DidChangeWorkspaceFoldersParams.t_of_yojson
    in
    ChangeWorkspaceFolders params
  | "workspace/didChangeConfiguration" ->
    let+ params = Json.message_params params DidChangeConfigurationParams.t_of_yojson in
    ChangeConfiguration params
  | "textDocument/willSave" ->
    let+ params = Json.message_params params WillSaveTextDocumentParams.t_of_yojson in
    WillSaveTextDocument params
  | "textDocument/didSave" ->
    let+ params = Json.message_params params DidSaveTextDocumentParams.t_of_yojson in
    DidSaveTextDocument params
  | "workspace/didChangeWatchedFiles" ->
    let+ params = Json.message_params params DidChangeWatchedFilesParams.t_of_yojson in
    DidChangeWatchedFiles params
  | "workspace/didCreateFiles" ->
    let+ params = Json.message_params params CreateFilesParams.t_of_yojson in
    DidCreateFiles params
  | "workspace/didDeleteFiles" ->
    let+ params = Json.message_params params DeleteFilesParams.t_of_yojson in
    DidDeleteFiles params
  | "workspace/didRenameFiles" ->
    let+ params = Json.message_params params RenameFilesParams.t_of_yojson in
    DidRenameFiles params
  | m when m = Cancel_request.meth_ ->
    let+ params = Json.message_params params Cancel_request.t_of_yojson in
    CancelRequest params
  | "window/workDoneProgress/cancel" ->
    let+ params = Json.message_params params WorkDoneProgressCancelParams.t_of_yojson in
    WorkDoneProgressCancel params
  | "$/setTrace" ->
    let+ params = Json.message_params params SetTraceParams.t_of_yojson in
    SetTrace params
  | "notebookDocument/didOpen" ->
    let+ params = Json.message_params params DidOpenNotebookDocumentParams.t_of_yojson in
    NotebookDocumentDidOpen params
  | "notebookDocument/didClose" ->
    let+ params = Json.message_params params DidCloseNotebookDocumentParams.t_of_yojson in
    NotebookDocumentDidClose params
  | "notebookDocument/didSave" ->
    let+ params = Json.message_params params DidSaveNotebookDocumentParams.t_of_yojson in
    NotebookDocumentDidSave params
  | "notebookDocument/didChange" ->
    let+ params =
      Json.message_params params DidChangeNotebookDocumentParams.t_of_yojson
    in
    NotebookDocumentDidChange params
  | m when m = Progress.method_ ->
    let+ params =
      Json.message_params params (ProgressParams.t_of_yojson Progress.t_of_yojson)
    in
    WorkDoneProgress params
  | _ -> Ok (UnknownNotification r)
;;

let to_jsonrpc t =
  let method_ = method_ t in
  let params = yojson_of_t t |> Option.map Jsonrpc.Structured.t_of_yojson in
  { Jsonrpc.Notification.params; method_ }
;;
