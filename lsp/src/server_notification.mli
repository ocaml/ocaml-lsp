open Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of ShowMessageParams.t
  | TelemetryNotification of Json.t
  | Unknown_notification of Jsonrpc.Request.t

val to_jsonrpc_request : t -> Jsonrpc.Request.t

val of_jsonrpc : Jsonrpc.Request.t -> (t, string) Result.t
