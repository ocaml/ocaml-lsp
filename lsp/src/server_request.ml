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

let method_ (type a) (t : a t) =
  match t with
  | WorkspaceConfiguration _ -> "workspace/configuration"
  | WorkspaceFolders -> "workspace/workspaceFolders"
  | WorkspaceApplyEdit _ -> "workspace/applyEdit"
  | ClientRegisterCapability _ -> "client/registerCapability"
  | ClientUnregisterCapability _ -> "client/unregisterCapability"
  | ShowMessageRequest _ -> "window/showMessageRequest"
