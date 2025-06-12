open Import

val command_name : string
val available : ShowDocumentClientCapabilities.t option -> bool
val command_run : _ Server.t -> ExecuteCommandParams.t -> Json.t Fiber.t
val for_uri : ShowDocumentClientCapabilities.t option -> Document.t -> CodeAction.t list
