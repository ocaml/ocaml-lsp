open! Import
open Types

module Progress = struct
  type t =
    | Begin of WorkDoneProgressBegin.t
    | Report of WorkDoneProgressReport.t
    | End of WorkDoneProgressEnd.t

  let yojson_of_t = function
    | Begin b -> WorkDoneProgressBegin.yojson_of_t b
    | Report r -> WorkDoneProgressReport.yojson_of_t r
    | End e -> WorkDoneProgressEnd.yojson_of_t e

  let t_of_yojson json =
    Json.Of.untagged_union "Progress"
      [ (fun j -> Begin (WorkDoneProgressBegin.t_of_yojson j))
      ; (fun j -> Report (WorkDoneProgressReport.t_of_yojson j))
      ; (fun j -> End (WorkDoneProgressEnd.t_of_yojson j))
      ]
      json
end

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of LogMessageParams.t
  | TelemetryNotification of Json.t
  | CancelRequest of Jsonrpc.Id.t
  | WorkDoneProgress of Progress.t ProgressParams.t
  | Unknown_notification of Jsonrpc.Message.notification

let method_ = function
  | ShowMessage _ -> "window/showMessage"
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"
  | LogMessage _ -> "window/logMessage"
  | TelemetryNotification _ -> "telemetry/event"
  | CancelRequest _ -> Cancel_request.meth_
  | WorkDoneProgress _ -> "$/progress"
  | Unknown_notification _ -> assert false

let yojson_of_t = function
  | LogMessage params -> LogMessageParams.yojson_of_t params
  | ShowMessage params -> ShowMessageParams.yojson_of_t params
  | PublishDiagnostics params -> PublishDiagnosticsParams.yojson_of_t params
  | TelemetryNotification params -> params
  | CancelRequest params -> Cancel_request.yojson_of_t params
  | WorkDoneProgress params ->
    (ProgressParams.yojson_of_t Progress.yojson_of_t) params
  | Unknown_notification _ -> assert false

let to_jsonrpc t =
  let method_ = method_ t in
  let params = Some (Jsonrpc.Message.Structured.of_json (yojson_of_t t)) in
  { Jsonrpc.Message.id = (); params; method_ }

let of_jsonrpc (r : Jsonrpc.Message.notification) =
  let open Result.O in
  match r.method_ with
  | "window/showMessage" ->
    let+ params = Json.message_params r ShowMessageParams.t_of_yojson in
    ShowMessage params
  | "textDocument/publishDiagnostics" ->
    let+ params = Json.message_params r PublishDiagnosticsParams.t_of_yojson in
    PublishDiagnostics params
  | "window/logMessage" ->
    let+ params = Json.message_params r LogMessageParams.t_of_yojson in
    LogMessage params
  | "telemetry/event" ->
    let+ params = Json.message_params r (fun x -> x) in
    TelemetryNotification params
  | "$/progress" ->
    let+ params =
      Json.message_params r (ProgressParams.t_of_yojson Progress.t_of_yojson)
    in
    WorkDoneProgress params
  | m when m = Cancel_request.meth_ ->
    let+ params = Json.message_params r Cancel_request.t_of_yojson in
    CancelRequest params
  | _ -> Ok (Unknown_notification r)
