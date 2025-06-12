open Import

val ocamllsp_source : string
val dune_source : string

type t

val create
  :  PublishDiagnosticsClientCapabilities.t option
  -> (PublishDiagnosticsParams.t list -> unit Fiber.t)
  -> display_merlin_diagnostics:bool
  -> shorten_merlin_diagnostics:bool
  -> client_name:string
  -> t

val send : t -> [ `All | `One of Uri.t ] -> unit Fiber.t

val set
  :  t
  -> [ `Dune of Drpc.V1.Diagnostic.Id.t * Uri.t * Diagnostic.t
     | `Merlin of Uri.t * Diagnostic.t list
     ]
  -> unit

val remove : t -> [ `Dune of Drpc.V1.Diagnostic.Id.t | `Merlin of Uri.t ] -> unit

val tags_of_message
  :  t
  -> src:[< `Dune | `Merlin ]
  -> string
  -> DiagnosticTag.t list option

(** Queries Merlin for diagnostics if [display_merlin_diagnostics] has been set to true
    either in [create] or with [set_display_merlin_diagnostics]; otherwise, acts as if
    Merlin returns [] *)
val merlin_diagnostics
  :  log_info:Lsp_timing_logger.t
  -> t
  -> Document.Merlin.t
  -> unit Fiber.t

val set_display_merlin_diagnostics : t -> display_merlin_diagnostics:bool -> unit

(** Checks if there was a previous call to [set] with this [t] that returned a nonempty
    list of [Diagnostic.t]s. *)
val has_cached_errors : t -> Document.Merlin.t -> bool

val set_shorten_merlin_diagnostics : t -> shorten_merlin_diagnostics:bool -> unit

(** Exposed for testing *)

val equal_message : string -> string -> bool
