open Import
open Protocol

type t =
  | PublishDiagnostics of PublishDiagnostics.params
  | ShowMessage of ShowMessage.Params.t
  | LogMessage of ShowMessage.Params.t
  | TelemetryNotification of Json.t

val to_jsonrpc_request : t -> Jsonrpc.Request.t
