open Import

val capability : string * Json.t
val meth : string
val get_doc_id : params:Jsonrpc.Structured.t option -> TextDocumentIdentifier.t option
val get_pos : params:Jsonrpc.Structured.t option -> Position.t option

val on_request
  :  log_info:Lsp_timing_logger.t
  -> params:Jsonrpc.Structured.t option
  -> State.t
  -> Json.t Fiber.t
