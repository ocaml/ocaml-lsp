open Import

module Request_params : sig
  type t

  val create
    :  ?verbosity:int
    -> text_document:Lsp.Types.TextDocumentIdentifier.t
    -> at:[ `Position of Position.t | `Range of Range.t ]
    -> index:int
    -> unit
    -> t

  val yojson_of_t : t -> Json.t
end

type t

val t_of_yojson : Json.t -> t
val capability : string * Json.t
val meth : string
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
