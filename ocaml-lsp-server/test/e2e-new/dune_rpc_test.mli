open Test.Import

module Signal : sig
  type t

  val wait : t -> unit Fiber.t
end

module Mailbox : sig
  type 'a t

  val wait : 'a t -> 'a Fiber.t
  val take_pending : 'a t -> 'a list
end

module Events : sig
  type t

  val dune_ready : t -> Signal.t
  val multiple_instances : t -> Signal.t
  val progress : t -> Lsp.Progress.t ProgressParams.t Mailbox.t

  val wait_for_diagnostics
    :  t
    -> f:(PublishDiagnosticsParams.t -> bool)
    -> PublishDiagnosticsParams.t Fiber.t

  val take_pending : t -> PublishDiagnosticsParams.t list
end

module Lifecycle_events : sig
  type t = private
    { dune : Events.t
    ; registrations : RegistrationParams.t Mailbox.t
    ; unregistrations : UnregistrationParams.t Mailbox.t
    }

  val create : unit -> t
  val handler : t -> unit Client.Handler.t
end

val for_uri : Uri.t -> PublishDiagnosticsParams.t -> bool
val has_dune_diagnostic : PublishDiagnosticsParams.t -> bool
val no_dune_diagnostic : PublishDiagnosticsParams.t -> bool
val only_dune_diagnostics : PublishDiagnosticsParams.t -> PublishDiagnosticsParams.t
val open_document : unit Client.t -> uri:Uri.t -> text:string -> unit Fiber.t

type project = private
  { temp : string
  ; root : string
  ; runtime_dir : string
  ; expected : string
  ; trigger : string
  ; gate : string
  ; old_source : string
  ; mutable dune_pid : int option
  }

val start_dune : ?build_dir:string -> string -> string -> int
val stop_process : int -> unit
val create_project : string -> project
val stop_dune : project -> unit
val destroy_project : project -> unit
val print_payload : project -> string -> Yojson.Safe.t -> unit
val print_payloads : project -> string -> ('a -> Yojson.Safe.t) -> 'a list -> unit

val run_with_workspace
  :  ?capabilities:ClientCapabilities.t
  -> root:string
  -> runtime_dir:string
  -> Lifecycle_events.t
  -> f:(unit Client.t -> WorkspaceFolder.t -> 'a Fiber.t)
  -> 'a

val run
  :  ?workspace_root:string
  -> ?capabilities:ClientCapabilities.t
  -> project
  -> Lifecycle_events.t
  -> f:(unit Client.t -> WorkspaceFolder.t -> 'a Fiber.t)
  -> 'a
