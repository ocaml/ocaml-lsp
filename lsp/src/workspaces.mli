open Types

(** State and fallback handling for LSP workspace folders. *)
type t

val create : InitializeParams.t -> t
val on_change : t -> DidChangeWorkspaceFoldersParams.t -> t
val workspace_folders : t -> WorkspaceFolder.t list
