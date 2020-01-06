open Import
open Protocol

class base :
  object
    method clientRegisterCapability : Registration.Params.t -> unit

    method clientUnregisterCapability : Unregistration.Params.t -> unit

    method logMessage : ShowMessage.Params.t -> unit

    method publishDiagnostics :
      PublishDiagnostics.publishDiagnosticsParams -> unit

    method showMessage : ShowMessage.Params.t -> unit

    method showMessageRequest :
      ShowMessage.Request.t -> Message.ActionItem.t option

    method telemetryNotification : json -> unit

    method workspaceApplyEdit :
      ApplyWorkspaceEdit.Params.t -> ApplyWorkspaceEdit.Response.t

    method workspaceConfiguration : Configuration.Params.t -> json list

    method workspaceFolders : WorkspaceFolder.t list

    (** The default implementation will do a simple handshake. Override this
        method to send your own initialization parameters. *)
    method on_initialize : Initialize.Result.t -> unit
  end

type t

val create : base -> Initialize.Params.t -> in_channel -> out_channel -> t

val notify : t -> Client_notification.t -> unit

val request : t -> 'a Client_request.t -> 'a
