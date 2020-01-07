open! Import
open Protocol

type t =
  | PublishDiagnostics of PublishDiagnostics.params
  | ShowMessage of ShowMessage.Params.t
  | LogMessage of ShowMessage.Params.t
  | TelemetryNotification of Json.t

let method_ = function
  | ShowMessage _ -> "window/showMessage"
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"
  | LogMessage _ -> "window/logMessage"
  | TelemetryNotification _ -> "telemetry/event"

let yojson_of_t = function
  | LogMessage params
  | ShowMessage params ->
    ShowMessage.Params.yojson_of_t params
  | PublishDiagnostics params -> PublishDiagnostics.yojson_of_params params
  | TelemetryNotification params -> params

let to_jsonrpc_request t =
  let method_ = method_ t in
  let params = Some (yojson_of_t t) in
  { Jsonrpc.Request.id = None; params; method_ }
