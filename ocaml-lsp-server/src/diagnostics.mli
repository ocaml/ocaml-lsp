open Import

type t

val create :
     (PublishDiagnosticsParams.t list -> unit Fiber.t)
  -> workspace_root:Uri.t Lazy.t
  -> t

val send : t -> [ `All | `One of Uri.t ] -> unit Fiber.t

val workspace_root : t -> Uri.t

module Dune : sig
  type t

  val gen : unit -> t
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
