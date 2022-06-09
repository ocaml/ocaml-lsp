open Import
open Types

module Progress : sig
  type t =
    | Begin of WorkDoneProgressBegin.t
    | Report of WorkDoneProgressReport.t
    | End of WorkDoneProgressEnd.t
end

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of LogMessageParams.t
  | TelemetryNotification of Json.t
  | CancelRequest of Jsonrpc.Id.t
  | WorkDoneProgress of Progress.t ProgressParams.t
  | UnknownNotification of Jsonrpc.Notification.t

val to_jsonrpc : t -> Jsonrpc.Notification.t

val of_jsonrpc : Jsonrpc.Notification.t -> (t, string) Result.t
