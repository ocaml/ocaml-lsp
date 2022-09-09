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
    Json.Of.untagged_union
      "Progress"
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
  | UnknownNotification of Jsonrpc.Notification.t

let method_ = function
  | ShowMessage _ -> "window/showMessage"
  | PublishDiagnostics _ -> "textDocument/publishDiagnostics"
  | LogMessage _ -> "window/logMessage"
  | TelemetryNotification _ -> "telemetry/event"
  | CancelRequest _ -> Cancel_request.meth_
  | WorkDoneProgress _ -> "$/progress"
  | UnknownNotification n -> n.method_

let yojson_of_t = function
  | LogMessage params -> Some (LogMessageParams.yojson_of_t params)
  | ShowMessage params -> Some (ShowMessageParams.yojson_of_t params)
  | PublishDiagnostics params ->
    Some (PublishDiagnosticsParams.yojson_of_t params)
  | TelemetryNotification params -> Some params
  | CancelRequest params -> Some (Cancel_request.yojson_of_t params)
  | WorkDoneProgress params ->
    Some ((ProgressParams.yojson_of_t Progress.yojson_of_t) params)
  | UnknownNotification n -> (n.params :> Json.t option)

let to_jsonrpc t =
  let method_ = method_ t in
  let params =
    match yojson_of_t t with
    | None -> None
    | Some s -> Some (Jsonrpc.Structured.t_of_yojson s)
  in
  { Jsonrpc.Notification.params; method_ }

let of_jsonrpc (r : Jsonrpc.Notification.t) =
  let open Result.O in
  let params = r.params in
  match r.method_ with
  | "window/showMessage" ->
    let+ params = Json.message_params params ShowMessageParams.t_of_yojson in
    ShowMessage params
  | "textDocument/publishDiagnostics" ->
    let+ params =
      Json.message_params params PublishDiagnosticsParams.t_of_yojson
    in
    PublishDiagnostics params
  | "window/logMessage" ->
    let+ params = Json.message_params params LogMessageParams.t_of_yojson in
    LogMessage params
  | "telemetry/event" ->
    let+ params = Json.message_params params (fun x -> x) in
    TelemetryNotification params
  | "$/progress" ->
    let+ params =
      Json.message_params
        params
        (ProgressParams.t_of_yojson Progress.t_of_yojson)
    in
    WorkDoneProgress params
  | m when m = Cancel_request.meth_ ->
    let+ params = Json.message_params params Cancel_request.t_of_yojson in
    CancelRequest params
  | _ -> Ok (UnknownNotification r)
