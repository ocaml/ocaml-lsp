open Import

(** A module to represent the field [initializationOptions] of the "object"
    [InitializeParams] sent from client to the server as a payload of
    "initialize" request *)
module Initialization_options : sig
  type merlin_construct_options = private { use_local_context : bool }

  type t = private { construct : merlin_construct_options }

  val default : t
end

(** OCaml LSP specific version of [Lsp.Types.InitializeParams], where we have
    [initializationOptions] not as an opaque [Json.t] but a specific type *)
module Initialize_params :
  Initialize_params
    with type t = InitializeParams.t
     and type initializationOptions := Initialization_options.t

type init =
  | Uninitialized
  | Initialized of InitializeParams.t

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

val initialization_params : t -> InitializeParams.t

val workspace_root : t -> Uri.t
