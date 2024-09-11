open Import

module Request_params : sig
  type t

  val yojson_of_t : t -> Json.t
  val create : TextDocumentIdentifier.t -> Position.t -> string -> t
end

type t

val t_of_yojson : Json.t -> t
val meth : string
val capability : string * [> `Bool of bool ]
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
