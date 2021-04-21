open Import

type init =
  | Uninitialized
  | Initialized of ClientCapabilities.t

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; scheduler : Scheduler.t
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; client_trace_verbosity : TraceValue.t
  ; tracer : Tracer.t option
  }
