open Import

module Request_params : sig
  type t

  val create : Uri.t -> t
  val yojson_of_t : t -> Json.t
end

type t

val t_of_yojson : Json.t -> t
val capability : string * Json.t
val meth : string
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
