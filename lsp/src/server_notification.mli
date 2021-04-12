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
  | Unknown_notification of Jsonrpc.Message.notification

val to_jsonrpc : t -> Jsonrpc.Message.notification

val of_jsonrpc : Jsonrpc.Message.notification -> (t, string) Result.t
