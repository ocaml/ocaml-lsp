open Import

module Request_params : sig
  type t

  val create
    :  text_document:TextDocumentIdentifier.t
    -> result_as_sexp:bool
    -> command:string
    -> args:string list
    -> t

  val yojson_of_t : t -> Json.t
end

type t

val t_of_yojson : Json.t -> t
val capability : string * Json.t
val meth : string
val get_doc_id : params:Jsonrpc.Structured.t option -> TextDocumentIdentifier.t option

val on_request
  :  log_info:Lsp_timing_logger.t
  -> params:Jsonrpc.Structured.t option
  -> State.t
  -> Json.t Fiber.t
