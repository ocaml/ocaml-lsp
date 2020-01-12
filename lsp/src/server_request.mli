open Import
open Protocol

type _ t =
  | WorkspaceApplyEdit :
      ApplyWorkspaceEdit.Params.t
      -> ApplyWorkspaceEdit.Response.t t
  | WorkspaceFolders : WorkspaceFolder.t list t
  | WorkspaceConfiguration : Configuration.Params.t -> Json.t list t
  | ClientRegisterCapability : Registration.Params.t -> unit t
  | ClientUnregisterCapability : Unregistration.Params.t -> unit t
  | ShowMessageRequest : ShowMessage.Request.t -> Message.ActionItem.t option t

val method_ : _ t -> string
