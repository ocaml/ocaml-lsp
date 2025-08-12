open Import

module Request_params : sig
  type t

  val create
    :  ?extract_name:string
    -> text_document:Lsp.Types.TextDocumentIdentifier.t
    -> start:Position.t
    -> stop:Position.t
    -> unit
    -> t

  val yojson_of_t : t -> Json.t
end

type t

val capability : string * Json.t
val meth : string
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
