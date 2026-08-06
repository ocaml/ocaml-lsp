(** Work-done progress lifecycle.

    [Lsp.Progress] defines the wire format of the [$/progress] notifications.
    This module manages the lifecycle of individual work-done progress tasks:
    minting a token, requesting its creation with [window/workDoneProgress/create],
    sending the [begin] notification, reporting incremental progress, and
    finally ending the task.

    Each {!start} returns an independent task handle. Any number of tasks may
    run concurrently; ending one task does not affect the others. Callers that
    want at most one active task (e.g. "end the previous task before starting
    a new one") must implement that policy themselves by keeping the handle of
    the running task and calling {!end_} on it before {!start}ing a new one.

    The transport is injected through callbacks, so this module is independent
    of any particular server implementation. *)

open Import
open Types

(** A work-done progress lifecycle. *)
type t

module Task : sig
  (** A handle to a running progress task. Obtained from {!start}. *)
  type t
end

(** [create ~create_task ~report_progress ()] creates a lifecycle that requests
    the creation of progress tasks with [create_task] and delivers progress
    notifications with [report_progress]. *)
val create
  :  create_task:(WorkDoneProgressCreateParams.t -> unit Fiber.t)
  -> report_progress:(Lsp.Progress.t ProgressParams.t -> unit Fiber.t)
  -> unit
  -> t

(** [start t ~token_name ~title ?message ()] mints a fresh token named after
    [token_name], requests its creation, and announces the beginning of the
    task. The returned handle identifies the task in subsequent {!report} and
    {!end_} calls.

    Raises if [create_task] fails, e.g. when the client rejects the creation
    request. *)
val start
  :  t
  -> token_name:string
  -> title:string
  -> ?message:string
  -> unit
  -> Task.t Fiber.t

(** [report t task ~percentage ~message] reports incremental progress of
    [Task.t]. *)
val report : t -> Task.t -> percentage:int -> message:string -> unit Fiber.t

(** [end_ t task ~message] announces the completion of [task]. The handle must
    not be used afterwards. *)
val end_ : t -> Task.t -> message:string -> unit Fiber.t
