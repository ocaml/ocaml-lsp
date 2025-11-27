(** Jsonrpc implementation *)

module Json : sig
  type t = Yojson.Safe.t

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

module Structured : sig
  type t =
    [ `Assoc of (string * Json.t) list
    | `List of Json.t list
    ]

  include Json.Jsonable.S with type t := t
end

module Notification : sig
  type t =
    { method_ : string
    ; params : Structured.t option
    }

  val create : ?params:Structured.t -> method_:string -> unit -> t
  val yojson_of_t : t -> Json.t
end

module Request : sig
  type t =
    { id : Id.t
    ; method_ : string
    ; params : Structured.t option
    }

  val create : ?params:Structured.t -> id:Id.t -> method_:string -> unit -> t
  val yojson_of_t : t -> Json.t
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
        | RequestFailed
        | ServerCancelled
        | ContentModified
        | RequestCancelled
        | Other of int
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

module Packet : sig
  type t =
    | Notification of Notification.t
    | Request of Request.t
    | Response of Response.t
    | Batch_response of Response.t list
    | Batch_call of [ `Request of Request.t | `Notification of Notification.t ] list

  include Json.Jsonable.S with type t := t
end
