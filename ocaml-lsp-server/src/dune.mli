open! Import

module Instance : sig
  type t

  val format_dune_file : t -> Document.t -> string Fiber.t
end

type t

val view_promotion_capability : string * Json.t

val run : t -> unit Fiber.t

val create :
     Workspaces.t
  -> ClientCapabilities.t
  -> Diagnostics.t
  -> Progress.t
  -> Document_store.t
  -> log:(type_:MessageType.t -> message:string -> unit Fiber.t)
  -> t

val update_workspaces : t -> Workspaces.t -> unit

val stop : t -> unit Fiber.t

val commands : string list

val on_command : t -> ExecuteCommandParams.t -> Json.t Fiber.t

val code_actions : t -> Uri.t -> CodeAction.t list

val for_doc : t -> Document.t -> Instance.t list
