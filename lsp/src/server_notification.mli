open Import
open Types

type t =
  | PublishDiagnostics of PublishDiagnosticsParams.t
  | ShowMessage of ShowMessageParams.t
  | LogMessage of LogMessageParams.t
  | LogTrace of LogTraceParams.t
  | TelemetryNotification of Json.t
  | CancelRequest of Jsonrpc.Id.t
  | WorkDoneProgress of Progress.t ProgressParams.t
  (** Typed work-done progress, used by decoding when conversion is lossless. *)
  | Progress of Json.t ProgressParams.t
  (** Raw progress values, including partial results and work-done-looking objects
      that would lose fields in typed conversion. Decoding uses payload shape, not
      token state: an exact work-done payload decodes as [WorkDoneProgress]. *)
  | UnknownNotification of Jsonrpc.Notification.t

val to_jsonrpc : t -> Jsonrpc.Notification.t
val of_jsonrpc : Jsonrpc.Notification.t -> (t, string) Result.t
