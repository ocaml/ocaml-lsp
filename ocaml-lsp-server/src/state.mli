open Import

type init =
  | Uninitialized
  | Initialized of
      { params : InitializeParams.t
      ; workspaces : Workspaces.t
      ; dune : Dune.t
      ; exp_client_caps : Client.Experimental_capabilities.t
      ; diagnostics : Diagnostics.t
      ; position_encoding : [ `UTF16 | `UTF8 ]
      }

(** State specific to the hoverExtended request. *)
type hover_extended =
  { mutable history : (Uri.t * Position.t * int) option
  (** File, position, and verbosity level of the last call to
      hoverExtended. This value is used to pick a verbosity level when it
      is not specific by the client. *)
  }

type t =
  { store : Document_store.t
  ; merlin : Document.Single_pipeline.t
  ; merlin_config : Merlin_config.DB.t
  ; init : init
  ; detached : Fiber.Pool.t
  ; configuration : Configuration.t
  ; trace : TraceValue.t
  ; ocamlformat_rpc : Ocamlformat_rpc.t
  ; symbols_thread : Lev_fiber.Thread.t Lazy_fiber.t
  ; wheel : Lev_fiber.Timer.Wheel.t
  ; hover_extended : hover_extended
  }

val create
  :  store:Document_store.t
  -> merlin:Lev_fiber.Thread.t
  -> detached:Fiber.Pool.t
  -> configuration:Configuration.t
  -> ocamlformat_rpc:Ocamlformat_rpc.t
  -> symbols_thread:Lev_fiber.Thread.t Lazy_fiber.t
  -> wheel:Lev_fiber.Timer.Wheel.t
  -> t

val position_encoding : t -> [ `UTF16 | `UTF8 ]
val wheel : t -> Lev_fiber.Timer.Wheel.t
val initialize_params : t -> InitializeParams.t

val initialize
  :  t
  -> position_encoding:[ `UTF16 | `UTF8 ]
  -> InitializeParams.t
  -> Workspaces.t
  -> Dune.t
  -> Diagnostics.t
  -> t

val workspace_root : t -> Uri.t
val workspaces : t -> Workspaces.t
val dune : t -> Dune.t
val modify_workspaces : t -> f:(Workspaces.t -> Workspaces.t) -> t

(** @return
      client capabilities passed from the client in [InitializeParams]; use
      [exp_client_caps] to get {i experimental} client capabilities.
    @raise Assertion_failure if the [t.init] is [Uninitialized] *)
val client_capabilities : t -> ClientCapabilities.t

(** @return experimental client capabilities *)
val experimental_client_capabilities : t -> Client.Experimental_capabilities.t

val diagnostics : t -> Diagnostics.t
val log_msg : t Server.t -> type_:MessageType.t -> message:string -> unit Fiber.t
