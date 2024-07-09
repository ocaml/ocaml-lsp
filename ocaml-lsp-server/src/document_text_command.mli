open Import

val command_name : string
val command_run : _ Server.t -> Document_store.t -> Json.t list option -> unit Fiber.t
