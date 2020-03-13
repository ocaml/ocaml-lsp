open Import
open Gprotocol

type _ t =
  | WorkspaceApplyEdit :
      ApplyWorkspaceEditParams.t
      -> ApplyWorkspaceEditResponse.t t
  | WorkspaceFolders : WorkspaceFolder.t list t
  | WorkspaceConfiguration : ConfigurationParams.t -> Json.t list t
  | ClientRegisterCapability : RegistrationParams.t -> unit t
  | ClientUnregisterCapability : UnregistrationParams.t -> unit t
  | ShowMessageRequest :
      ShowMessageRequestParams.t
      -> MessageActionItem.t option t

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
  | WorkspaceApplyEdit params -> ApplyWorkspaceEditParams.yojson_of_t params
  | WorkspaceFolders -> `Null
  | WorkspaceConfiguration params -> ConfigurationParams.yojson_of_t params
  | ClientRegisterCapability params -> RegistrationParams.yojson_of_t params
  | ClientUnregisterCapability params -> UnregistrationParams.yojson_of_t params
  | ShowMessageRequest params -> ShowMessageRequestParams.yojson_of_t params

let to_jsonrpc_request t ~id =
  let method_ = method_ t in
  let params = params t in
  Jsonrpc.Request.create ~id ~method_ ~params ()
