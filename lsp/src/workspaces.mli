open Types

(** State and fallback handling for LSP workspace folders. *)
type t

val create : InitializeParams.t -> t
val on_change : t -> DidChangeWorkspaceFoldersParams.t -> t
val workspace_folders : t -> WorkspaceFolder.t list

(** Return the innermost workspace folder containing the document. *)
val find_workspace_folder : t -> DocumentUri.t -> WorkspaceFolder.t option
