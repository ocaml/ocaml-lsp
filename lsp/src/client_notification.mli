open! Import
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
  | LogTrace of LogTraceParams.t
  | SetTrace of SetTraceParams.t
  | UnknownNotification of Jsonrpc.Notification.t

val of_jsonrpc : Jsonrpc.Notification.t -> (t, string) result

val to_jsonrpc : t -> Jsonrpc.Notification.t
