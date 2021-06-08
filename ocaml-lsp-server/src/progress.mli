open Import

type t

val create :
     ClientCapabilities.t
  -> report_progress:
       (Server_notification.Progress.t ProgressParams.t -> unit Fiber.t)
  -> create_task:(WorkDoneProgressCreateParams.t -> unit Fiber.t)
  -> t

val end_build_if_running : t -> unit Fiber.t

val build_event : t -> Drpc.Build.Event.t -> unit Fiber.t

val build_progress : t -> Drpc.Progress.t -> unit Fiber.t

val should_report_build_progress : t -> bool
