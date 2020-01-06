open Import
open Protocol

module State = struct
  type t =
    | Initializing of Initialize.Params.t
    | Initialized of Initialize.Result.t
    | Closed
end

class type handler =
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

type t =
  { mutable state : State.t
  ; ic : in_channel
  ; oc : out_channel
  ; handler : handler
  }

let request _ (type a) (_ : a Client_request.t) : a = assert false

let notify _ _ = assert false

let check_init = function
  | None -> failwith "The client has not been initialized yet"
  | Some s -> s

class base =
  let notify (t : t option) _ =
    let t = check_init t in
    notify t
  in
  let request (t : t option) (type a) (req : a Client_request.t) : a =
    let t = check_init t in
    request t req
  in
  object (self)
    val t = None

    method publishDiagnostics (_ : PublishDiagnostics.params) = ()

    method showMessage p = self#logMessage p

    method logMessage (_ : ShowMessage.Params.t) = ()

    method telemetryNotification (_ : json) = ()

    method workspaceApplyEdit (_ : ApplyWorkspaceEdit.Params.t)
        : ApplyWorkspaceEdit.Response.t =
      assert false

    method workspaceFolders : WorkspaceFolder.t list = []

    method workspaceConfiguration (_ : Configuration.Params.t) : json list = []

    method clientRegisterCapability (_ : Registration.Params.t) = ()

    method clientUnregisterCapability (_ : Unregistration.Params.t) = ()

    method showMessageRequest (_ : ShowMessage.Request.t)
        : Message.ActionItem.t option =
      None

    method on_initialize (_ : Initialize.Result.t) = ()
  end

let create (handler : base) params ic oc =
  let t = { state = State.Initializing params; ic; oc; handler } in
  let result = request t (Client_request.Initialize params) in
  t.state <- State.Initialized result;
  t.handler#on_initialize result;
  t
