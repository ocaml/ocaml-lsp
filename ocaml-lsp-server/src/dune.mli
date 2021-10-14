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

val commands : string list

val on_command : t -> ExecuteCommandParams.t -> Json.t Fiber.t

val code_actions : t -> Document.t -> CodeAction.t list
