open Import

type t

val create : InitializeParams.t -> t
val on_change : t -> DidChangeWorkspaceFoldersParams.t -> t
val workspace_folders : t -> WorkspaceFolder.t list
