open! Import

type t

val view_promotion_capability : string * Json.t

val run : t -> unit Fiber.t

val create :
     Workspaces.t
  -> ClientCapabilities.t
  -> Diagnostics.t
  -> Progress.t
  -> log:(type_:MessageType.t -> message:string -> unit Fiber.t)
  -> t Fiber.t

val update_workspaces : t -> Workspaces.t -> unit

val stop : t -> unit Fiber.t
