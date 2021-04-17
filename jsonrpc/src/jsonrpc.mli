(** Jsonrpc implementation *)

module Json : sig
  type t =
    [ `Assoc of (string * t) list
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `Intlit of string
    | `List of t list
    | `Null
    | `String of string
    | `Tuple of t list
    | `Variant of string * t option
    ]

  (** Raised when conversions from json fail *)
  exception Of_json of (string * t)

  module Jsonable : sig
    module type S = sig
      type json

      type t

      val yojson_of_t : t -> json

      val t_of_yojson : json -> t
    end
    with type json := t
  end
end

module Id : sig
  type t =
    [ `String of string
    | `Int of int
    ]

  include Json.Jsonable.S with type t := t

  val hash : t -> int

  val equal : t -> t -> bool
end

module Message : sig
  module Structured : sig
    type t =
      [ `Assoc of (string * Json.t) list
      | `List of Json.t list
      ]

    val of_json : Json.t -> t

    val to_json : t -> Json.t
  end

  type 'id t =
    { id : 'id
    ; method_ : string
    ; params : Structured.t option
    }

  val create : ?params:Structured.t -> id:'id -> method_:string -> unit -> 'id t

  type request = Id.t t

  type notification = unit t

  type either = Id.t option t

  val either_of_yojson : Json.t -> either

  val yojson_of_either : either -> Json.t

  val yojson_of_notification : notification -> Json.t

  val yojson_of_request : request -> Json.t
end

module Response : sig
  module Error : sig
    module Code : sig
      type t =
        | ParseError
        | InvalidRequest
        | MethodNotFound
        | InvalidParams
        | InternalError
        | ServerErrorStart
        | ServerErrorEnd
        | ServerNotInitialized
        | UnknownErrorCode
        | RequestCancelled
        | ContentModified
    end

    type t =
      { code : Code.t
      ; message : string
      ; data : Json.t option
      }

    exception E of t

    val make : ?data:Json.t -> code:Code.t -> message:string -> unit -> t

    val raise : t -> 'a

    val of_exn : exn -> t

    val yojson_of_t : t -> Json.t
  end

  type t =
    { id : Id.t
    ; result : (Json.t, Error.t) Result.t
    }

  val ok : Id.t -> Json.t -> t

  val error : Id.t -> Error.t -> t

  include Json.Jsonable.S with type t := t
end

type packet =
  | Message of Id.t option Message.t
  | Response of Response.t

val yojson_of_packet : packet -> Json.t
