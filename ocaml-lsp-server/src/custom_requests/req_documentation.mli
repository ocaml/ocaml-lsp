open Import

val meth : string

val capability : string * [> `Bool of bool ]

module GetDocClientCapabilities : sig
  type t = { contentFormat : MarkupKind.t list }

  val yojson_of_t : t -> [> `Assoc of [> `List of Json.t list ] ]
end

module GetDoc : sig
  type t = { doc : MarkupContent.t }
end

type t = GetDoc.t

val on_request :
     params:[< Yojson.Safe.t > `Assoc ] option
  -> State.t
  -> [> `Assoc of (string * Yojson.Safe.t) list | `Null ] Fiber.t
