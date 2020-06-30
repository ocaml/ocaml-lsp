open! Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of ShowMessageParams.t
  | TelemetryNotification of Json.t
  | Unknown_notification of Jsonrpc.Message.notification

let method_ = function
  | ShowMessage _ -> "window/showMessage"
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"
  | LogMessage _ -> "window/logMessage"
  | TelemetryNotification _ -> "telemetry/event"
  | Unknown_notification _ -> assert false

let yojson_of_t = function
  | LogMessage params
  | ShowMessage params ->
    ShowMessageParams.yojson_of_t params
  | PublishDiagnostics params -> PublishDiagnosticsParams.yojson_of_t params
  | TelemetryNotification params -> params
  | Unknown_notification _ -> assert false

let to_jsonrpc t =
  let method_ = method_ t in
  let params = Some (yojson_of_t t) in
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
  | _ -> Ok (Unknown_notification r)
