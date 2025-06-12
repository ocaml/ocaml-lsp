open Import

val capability : string * Json.t
val meth : string
val get_doc_id : params:Jsonrpc.Structured.t option -> TextDocumentIdentifier.t option
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t
