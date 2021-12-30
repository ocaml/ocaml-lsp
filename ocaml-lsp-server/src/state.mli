open Import

type init =
  | Uninitialized
  | Initialized of
      { params : InitializeParams.t
      ; workspaces : Workspaces.t
      ; dune : Dune.t
      }

type t =
  { store : Document_store.t
  ; merlin : Lev_fiber.Thread.t
  ; merlin_config : Merlin_config.t
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat : Ocamlformat.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; diagnostics : Diagnostics.t
  ; symbols_thread : Lev_fiber.Thread.t Lazy_fiber.t
  ; wheel : Lev_fiber.Timer.Wheel.t
  }

val create :
     store:Document_store.t
  -> merlin:Lev_fiber.Thread.t
  -> detached:Fiber.Pool.t
  -> configuration:Configuration.t
  -> ocamlformat:Ocamlformat.t
  -> ocamlformat_rpc:Ocamlformat_rpc.t
  -> diagnostics:Diagnostics.t
  -> symbols_thread:Lev_fiber.Thread.t Lazy_fiber.t
  -> wheel:Lev_fiber.Timer.Wheel.t
  -> t

val wheel : t -> Lev_fiber.Timer.Wheel.t

val initialize_params : t -> InitializeParams.t

val initialize : t -> InitializeParams.t -> Workspaces.t -> Dune.t -> t

val workspace_root : t -> Uri.t

val workspaces : t -> Workspaces.t

val dune : t -> Dune.t

val modify_workspaces : t -> f:(Workspaces.t -> Workspaces.t) -> t
