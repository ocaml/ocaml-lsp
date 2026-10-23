open Import

(** This module handles [ocamllsp/completePrefixAtPos] requests. *)

module Request_params : sig
  type t

  val create
    :  text_document:TextDocumentIdentifier.t
    -> position:Position.t
    -> prefix:string
    -> t

  val yojson_of_t : t -> Json.t
end

val capability : string * Json.t
val meth : string
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t

module For_testing : sig
  val serialize : CompletionItem.t list * (string * string) list option -> Json.t
end
