open! Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of ShowMessageParams.t
  | TelemetryNotification of Json.t
  | CancelRequest of Jsonrpc.Id.t
  | WorkDoneProgressCancel of WorkDoneProgressCancelParams.t
  | Unknown_notification of Jsonrpc.Message.notification

let method_ = function
  | ShowMessage _ -> "window/showMessage"
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"
  | LogMessage _ -> "window/logMessage"
  | TelemetryNotification _ -> "telemetry/event"
  | CancelRequest _ -> Cancel_request.meth_
  | WorkDoneProgressCancel _ -> "window/workDoneProgress/cancel"
  | Unknown_notification _ -> assert false

let yojson_of_t = function
  | LogMessage params
  | ShowMessage params ->
    ShowMessageParams.yojson_of_t params
  | PublishDiagnostics params -> PublishDiagnosticsParams.yojson_of_t params
  | TelemetryNotification params -> params
  | CancelRequest params -> Cancel_request.yojson_of_t params
  | WorkDoneProgressCancel params ->
    WorkDoneProgressCancelParams.yojson_of_t params
  | Unknown_notification _ -> assert false

let to_jsonrpc t =
  let method_ = method_ t in
  let params = Some (Jsonrpc.Message.Structured.of_json (yojson_of_t t)) in
  { Jsonrpc.Message.id = (); params; method_ }

let of_jsonrpc (r : Jsonrpc.Message.notification) =
  let open Result.O in
  match r.method_ with
  | "window/showMessage" ->
    let+ params = Jsonrpc.Message.params r ShowMessageParams.t_of_yojson in
    ShowMessage params
  | "textDocument/publishDiagnostics" ->
    let+ params =
      Jsonrpc.Message.params r PublishDiagnosticsParams.t_of_yojson
    in
    PublishDiagnostics params
  | "window/logMessage" ->
    let+ params = Jsonrpc.Message.params r ShowMessageParams.t_of_yojson in
    LogMessage params
  | "telemetry/event" ->
    let+ params = Jsonrpc.Message.params r (fun x -> x) in
    TelemetryNotification params
  | m when m = Cancel_request.meth_ ->
    let+ params = Jsonrpc.Message.params r Cancel_request.t_of_yojson in
    CancelRequest params
  | "window/workDoneProgress/cancel" ->
    let+ params =
      Jsonrpc.Message.params r WorkDoneProgressCancelParams.t_of_yojson
    in
    WorkDoneProgressCancel params
  | _ -> Ok (Unknown_notification r)
