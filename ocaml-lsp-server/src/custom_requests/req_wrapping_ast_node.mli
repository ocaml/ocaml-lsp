open Import

val capability : string * Json.t
val meth : string

val on_request :
  params:Jsonrpc.Message.Structured.t option -> State.t -> Json.t Fiber.t
