(** Jsonrpc implementation *)
open Import

module Id : sig
  type t = (string, int) Either.t

  include Json.Jsonable.S with type t := t
end

module Request : sig
  type t =
    { id : Id.t option
    ; method_ : string
    ; params : Json.t option
    }

  include Json.Jsonable.S with type t := t

  val params : t -> (Json.t -> 'a) -> ('a, string) Result.t

  val create : ?id:Id.t -> ?params:Json.t -> method_:string -> unit -> t
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

    val of_exn : Exn.t -> t

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
  | Request of Request.t
  | Response of Response.t

val yojson_of_packet : packet -> Json.t

(** IO free implementation of the jsonrpc protocol. We stay completely agnostic
    of transport by only dealing with abstract jsonrpc packets *)
module Session (Chan : sig
  type t

  val send : t -> packet -> unit Fiber.t

  val recv : t -> packet option Fiber.t

  val close : t -> unit Fiber.t
end) : sig
  type t

  val create :
       ?on_request:(Request.t -> Response.t Fiber.t)
    -> ?on_notification:(Request.t -> unit Fiber.t)
    -> name:string
    -> Chan.t
    -> t

  val stop : t -> unit Fiber.t

  val stopped : t -> unit Fiber.t

  val run : t -> unit Fiber.t

  val notification : t -> Request.t -> unit Fiber.t

  (** TODO this isn't type safe enough. Request.t must require an ID*)
  val request : t -> Request.t -> Response.t Fiber.t
end
