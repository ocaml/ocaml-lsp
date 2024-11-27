open Import

val command_name : string
val available : ShowDocumentClientCapabilities.t option -> bool
val command_run : 'a Server.t -> ExecuteCommandParams.t -> Json.t Fiber.t

val code_actions
  :  Document.t
  -> CodeActionParams.t
  -> ShowDocumentClientCapabilities.t option
  -> CodeAction.t list Fiber.t
