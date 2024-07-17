open Import

type t

val create
  :  ClientCapabilities.t
  -> report_progress:(Lsp.Progress.t ProgressParams.t -> unit Fiber.t)
  -> create_task:(WorkDoneProgressCreateParams.t -> unit Fiber.t)
  -> t

val end_build_if_running : t -> unit Fiber.t
val build_progress : t -> Drpc.Progress.t -> unit Fiber.t
