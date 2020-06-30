(** Jsonrpc implementation *)
open Import

module Id : sig
  type t = (string, int) Either.t

  include Json.Jsonable.S with type t := t
end

module Message : sig
  type 'id t =
    { id : 'id
    ; method_ : string
    ; params : Json.t option
    }

  val params : _ t -> (Json.t -> 'a) -> ('a, string) Result.t

  val create : ?params:Json.t -> id:'id -> method_:string -> unit -> 'id t

  type request = Id.t t

  type notification = unit t

  type either = Id.t option t

  val either_of_yojson : Json.t -> either

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
  | Message of Id.t option Message.t
  | Response of Response.t

val yojson_of_packet : packet -> Json.t

module Notify : sig
  type t =
    | Stop
    | Continue
end

(** IO free implementation of the jsonrpc protocol. We stay completely agnostic
    of transport by only dealing with abstract jsonrpc packets *)
module Session (Chan : sig
  type t

  val send : t -> packet -> unit Fiber.t

  val recv : t -> packet option Fiber.t

  val close : t -> unit Fiber.t
end) : sig
  type 'state t

  module Context : sig
    type ('state, 'req) t

    type 'a session

    val message : (_, 'req) t -> 'req Message.t

    val state : ('a, _) t -> 'a

    val session : ('a, _) t -> 'a session
  end
  with type 'a session := 'a t

  val create :
       ?on_request:(('state, Id.t) Context.t -> (Response.t * 'state) Fiber.t)
    -> ?on_notification:
         (('state, unit) Context.t -> (Notify.t * 'state) Fiber.t)
    -> name:string
    -> Chan.t
    -> 'state
    -> 'state t

  val state : 'a t -> 'a

  val stop : _ t -> unit Fiber.t

  val stopped : _ t -> unit Fiber.t

  val run : _ t -> unit Fiber.t

  val notification : _ t -> Message.notification -> unit Fiber.t

  val request : _ t -> Message.request -> Response.t Fiber.t
end
