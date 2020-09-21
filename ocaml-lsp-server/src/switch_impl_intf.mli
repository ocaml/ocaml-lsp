open Import

val capability : string * Json.t

val meth : string

val on_request :
     params:Json.t option
  -> State.t
  -> (Json.t * State.t, Jsonrpc.Response.Error.t) result Fiber.t
