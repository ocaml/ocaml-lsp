open Import

val action : string

val code_action :
     Document.t
  -> Document_store.t
  -> (CodeAction.t option, Jsonrpc.Response.Error.t) Result.t Fiber.t
