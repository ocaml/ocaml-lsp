open Import

module Request_params : sig
  type t

  val yojson_of_t : t -> Json.t

  val create
    :  TextDocumentIdentifier.t
    -> Position.t
    -> int
    -> string
    -> bool
    -> MarkupKind.t option
    -> t
end

type t

val meth : string
val capability : string * [> `Bool of bool ]
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
