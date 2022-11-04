open Import

val capability : string * Json.t

val meth : string

val on_request : params:Jsonrpc.Structured.t option -> Json.t
