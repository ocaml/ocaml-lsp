open Import

val ocamllsp_source : string

val dune_source : string

type t

val create :
     (PublishDiagnosticsParams.t list -> unit Fiber.t)
  -> workspace_root:Uri.t Lazy.t
  -> t

val send : t -> [ `All | `One of Uri.t ] -> unit Fiber.t

val workspace_root : t -> Uri.t

module Dune : sig
  type t

  val gen : Pid.t -> t
end

val set :
     t
  -> [ `Dune of Dune.t * Drpc.Diagnostic.Id.t * Uri.t * Diagnostic.t
     | `Merlin of Uri.t * Diagnostic.t list
     ]
  -> unit

val remove :
  t -> [ `Dune of Dune.t * Drpc.Diagnostic.Id.t | `Merlin of Uri.t ] -> unit

val disconnect : t -> Dune.t -> unit

val tags_of_message :
  src:[< `Dune | `Merlin ] -> string -> DiagnosticTag.t list option

(** Exposed for testing *)

val equal_message : string -> string -> bool
