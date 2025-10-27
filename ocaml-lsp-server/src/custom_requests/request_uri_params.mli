open Import

type t = Uri.t

val yojson_of_t : t -> Json.t
val parse_exn : Jsonrpc.Structured.t option -> t
