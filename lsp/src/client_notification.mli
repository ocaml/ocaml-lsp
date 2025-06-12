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
  | SetTrace of SetTraceParams.t
  | WorkDoneProgress of Progress.t ProgressParams.t
  | NotebookDocumentDidOpen of DidOpenNotebookDocumentParams.t
  | NotebookDocumentDidChange of DidChangeNotebookDocumentParams.t
  | NotebookDocumentDidSave of DidSaveNotebookDocumentParams.t
  | NotebookDocumentDidClose of DidCloseNotebookDocumentParams.t
  | UnknownNotification of Jsonrpc.Notification.t

(** This is a to_string function. It's exposed for use in logging. *)
val method_ : t -> string

val of_jsonrpc : Jsonrpc.Notification.t -> (t, string) result
val to_jsonrpc : t -> Jsonrpc.Notification.t
val all_uris : t -> Uri0.t list
