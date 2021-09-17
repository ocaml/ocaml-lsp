open Import

type t

val create :
     (PublishDiagnosticsParams.t list -> unit Fiber.t)
  -> workspace_root:Uri.t Lazy.t
  -> t

val send : t -> unit Fiber.t

type dune_status =
  | Inactive
  | Connected
  | Disconnected

val update_dune_status : t -> dune_status -> unit

val workspace_root : t -> Uri.t

val set :
     t
  -> [ `Dune of Drpc.Diagnostic.Id.t * Uri.t * Diagnostic.t
     | `Merlin of Uri.t * Diagnostic.t list
     ]
  -> unit

val remove : t -> [ `Dune of Drpc.Diagnostic.Id.t | `Merlin of Uri.t ] -> unit
