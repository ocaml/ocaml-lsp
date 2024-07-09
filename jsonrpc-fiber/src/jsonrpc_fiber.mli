module Notify : sig
  type t =
    | Stop
    | Continue
end

module Reply : sig
  type t

  val now : Jsonrpc.Response.t -> t
  val later : ((Jsonrpc.Response.t -> unit Fiber.t) -> unit Fiber.t) -> t
end

(** Raised when the server is shutdown and a pending request will not complete. *)
exception Stopped of Jsonrpc.Request.t

(** IO free implementation of the jsonrpc protocol. We stay completely agnostic
    of transport by only dealing with abstract jsonrpc packets *)
module Make (Chan : sig
    type t

    val send : t -> Jsonrpc.Packet.t list -> unit Fiber.t
    val recv : t -> Jsonrpc.Packet.t option Fiber.t
    val close : t -> [ `Read | `Write ] -> unit Fiber.t
  end) : sig
  type 'state t

  module Context : sig
    type 'a session := 'a t
    type ('state, 'message) t

    val message : (_, 'message) t -> 'message
    val state : ('a, _) t -> 'a
    val session : ('a, _) t -> 'a session
  end

  val create
    :  ?on_request:(('state, Jsonrpc.Request.t) Context.t -> (Reply.t * 'state) Fiber.t)
    -> ?on_notification:
         (('state, Jsonrpc.Notification.t) Context.t -> (Notify.t * 'state) Fiber.t)
    -> name:string
    -> Chan.t
    -> 'state
    -> 'state t

  val state : 'a t -> 'a
  val stop : _ t -> unit Fiber.t
  val stopped : _ t -> unit Fiber.t
  val run : _ t -> unit Fiber.t
  val notification : _ t -> Jsonrpc.Notification.t -> unit Fiber.t
  val request : _ t -> Jsonrpc.Request.t -> Jsonrpc.Response.t Fiber.t

  type cancel

  val fire : cancel -> unit Fiber.t

  val request_with_cancel
    :  _ t
    -> Jsonrpc.Request.t
    -> cancel * [ `Ok of Jsonrpc.Response.t | `Cancelled ] Fiber.t

  module Batch : sig
    type t

    val create : unit -> t
    val notification : t -> Jsonrpc.Notification.t -> unit

    type response

    val await : response -> Jsonrpc.Response.t Fiber.t
    val request : t -> Jsonrpc.Request.t -> response
  end

  val submit : _ t -> Batch.t -> unit Fiber.t
end
