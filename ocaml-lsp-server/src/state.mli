open Import

type init =
  | Uninitialized
  | Initialized of InitializeParams.t

module Uri_map : Map.S with type key = Uri.t

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; workspace_folders : WorkspaceFolder.t Uri_map.t option
  ; trace : TraceValue.t
  ; ocamlformat : Fmt.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; diagnostics : Diagnostics.t
  ; symbols_thread : Scheduler.thread Lazy_fiber.t
  }

val workspace_root : t -> Uri.t
