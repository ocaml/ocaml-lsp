open Import

val capability : string * Json.t

val meth : string

val get_intf_impl_counterparts : string -> string list

val on_request :
     params:Json.t option
  -> State.t
  -> (Json.t * State.t, Jsonrpc.Response.Error.t) result Fiber.t
