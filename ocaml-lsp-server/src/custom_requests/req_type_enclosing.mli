open Import

module Request_params : sig
  type t

  val create :
       ?range_end:Position.t
    -> ?verbosity:int
    -> text_document_position:Lsp.Types.TextDocumentPositionParams.t
    -> index:int
    -> unit
    -> t

  val yojson_of_t : t -> Json.t
end

type t

val capability : string * Json.t

val meth : string

val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
