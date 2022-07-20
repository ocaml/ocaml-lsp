open Import

val command_name : string

val command_run : _ Server.t -> ExecuteCommandParams.t -> Json.t Fiber.t

val for_uri : DocumentUri.t -> CodeAction.t list
