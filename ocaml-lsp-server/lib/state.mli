open Import

type init =
  | Uninitialized
  | Initialized of InitializeParams.t * Workspaces.t

type t =
  { store : Document_store.t
  ; merlin : Scheduler.thread
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat : Fmt.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; diagnostics : Diagnostics.t
  ; symbols_thread : Scheduler.thread Lazy_fiber.t
  }

val initialize_params : t -> InitializeParams.t

val initialize : t -> InitializeParams.t -> t

val workspace_root : t -> Uri.t

val workspaces : t -> Workspaces.t

val modify_workspaces : t -> f:(Workspaces.t -> Workspaces.t) -> t
