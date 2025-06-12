open Import

module Request_params : sig
  type t

  val create : Uri.t -> t
  val yojson_of_t : t -> Json.t
end

val get_doc_id : params:Jsonrpc.Structured.t option -> TextDocumentIdentifier.t option
val get_pos : params:Jsonrpc.Structured.t option -> Position.t option

type t

val t_of_yojson : Json.t -> t
val capability : string * Json.t
val jump_capability : string * Json.t
val meth : string
val jump : string

(** Sends back a list of typed-hole locations.
    @param params [{ "uri":<uri> }] *)
val on_request
  :  log_info:Lsp_timing_logger.t
  -> params:Jsonrpc.Structured.t option
  -> State.t
  -> Json.t Fiber.t

(** Sends back a range, allowing the client to jump to the next/previous typed hole.
    @param params [{ "uri":<uri>, "position":<position>, "direction":"prev"|"next" }] *)
val on_jump_request
  :  log_info:Lsp_timing_logger.t
  -> params:Jsonrpc.Structured.t option
  -> State.t
  -> Json.t Fiber.t
