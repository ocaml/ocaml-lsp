open Import

type init =
  | Uninitialized
  | Initialized of ClientCapabilities.t

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; client_trace_verbosity : TraceValue.t
  ; ocamlformat : Fmt.t
  ; dune : Dune.t
  }
