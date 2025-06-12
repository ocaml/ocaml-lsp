open Import

module Request_params : sig
  type t =
    { text_document : Import.TextDocumentIdentifier.t
    ; cursor_position : Position.t
    ; verbosity : int Import.Json.Nullable_option.t
    }

  val create
    :  ?verbosity:int
    -> text_document:Lsp.Types.TextDocumentIdentifier.t
    -> cursor_position:Position.t
    -> unit
    -> t

  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
  val params_schema : [> `Assoc of (string * [> `String of string ]) list ]
  val of_jsonrpc_params : Jsonrpc.Structured.t -> t option
  val of_jsonrpc_params_exn : Jsonrpc.Structured.t option -> t
end

type t

val t_of_yojson : Json.t -> t
val capability : string * Json.t
val meth : string
val get_doc_id : params:Jsonrpc.Structured.t option -> TextDocumentIdentifier.t option
val get_pos : params:Jsonrpc.Structured.t option -> Position.t option

val on_request
  :  log_info:Lsp_timing_logger.t
  -> params:Jsonrpc.Structured.t option
  -> State.t Server.t
  -> Json.t Fiber.t
