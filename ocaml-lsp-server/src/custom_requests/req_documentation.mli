val meth : string

val capability : string * [> `Bool of bool ]

module GetDocClientCapabilities : sig
  type t = { contentFormat : Import.MarkupKind.t list option }

  val yojson_of_t :
    t -> [> `Assoc of (string * [> `List of Yojson.Safe.t list ]) list ]

  val t_of_yojson : Yojson.Safe.t -> t
end

module GetDocParams : sig
  type t =
    { text_document : Import.TextDocumentIdentifier.t
    ; position : Position.t
    ; identifier : string option
    ; contentFormat : Import.MarkupKind.t option
    }

  val yojson_of_t : t -> [> `Assoc of (string * Yojson.Safe.t) list ]

  val t_of_yojson : Yojson.Safe.t -> t
end

module GetDoc : sig
  type t = { doc : Import.MarkupContent.t }

  val yojson_of_t : t -> [> `Assoc of (string * Yojson.Safe.t) list ]

  val t_of_yojson : Yojson.Safe.t -> t
end

val on_request :
     params:[< Yojson.Safe.t > `Assoc ] option
  -> State.t
  -> [> `Assoc of (string * Yojson.Safe.t) list | `Null ] Fiber.t
