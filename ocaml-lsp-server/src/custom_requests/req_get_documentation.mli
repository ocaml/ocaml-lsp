open Import

module Request_params : sig
  type t

  val yojson_of_t : t -> Json.t

  val create
    :  text_document:TextDocumentIdentifier.t
    -> position:Position.t
    -> ?identifier:string option
    -> ?contentFormat:MarkupKind.t option
    -> unit
    -> t
end

type t

val t_of_yojson : Json.t -> t
val meth : string
val capability : string * [> `Bool of bool ]
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
