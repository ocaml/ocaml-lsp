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

let params (type a) (t : a t) =
  match t with
  | WorkspaceApplyEdit params -> ApplyWorkspaceEdit.Params.yojson_of_t params
  | WorkspaceFolders -> `Null
  | WorkspaceConfiguration params -> Configuration.Params.yojson_of_t params
  | ClientRegisterCapability params -> Registration.Params.yojson_of_t params
  | ClientUnregisterCapability params ->
    Unregistration.Params.yojson_of_t params
  | ShowMessageRequest params -> ShowMessage.Request.yojson_of_t params

let to_jsonrpc_request t ~id =
  let method_ = method_ t in
  let params = params t in
  Jsonrpc.Request.create ~id ~method_ ~params ()
