open Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of ShowMessageParams.t
  | TelemetryNotification of Json.t

val to_jsonrpc_request : t -> Jsonrpc.Request.t
