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
exception Stopped of Jsonrpc.Message.request

(** IO free implementation of the jsonrpc protocol. We stay completely agnostic
    of transport by only dealing with abstract jsonrpc packets *)
module Make (Chan : sig
  type t

  val send : t -> Jsonrpc.packet list -> unit Fiber.t

  val recv : t -> Jsonrpc.packet option Fiber.t

  val close : t -> [ `Read | `Write ] -> unit Fiber.t
end) : sig
  type 'state t

  module Context : sig
    type ('state, 'req) t

    type 'a session

    val message : (_, 'req) t -> 'req Jsonrpc.Message.t

    val state : ('a, _) t -> 'a

    val session : ('a, _) t -> 'a session
  end
  with type 'a session := 'a t

  val create :
       ?on_request:
         (('state, Jsonrpc.Id.t) Context.t -> (Reply.t * 'state) Fiber.t)
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

  val notification : _ t -> Jsonrpc.Message.notification -> unit Fiber.t

  val request : _ t -> Jsonrpc.Message.request -> Jsonrpc.Response.t Fiber.t

  module Batch : sig
    type t

    val create : unit -> t

    val notification : t -> Jsonrpc.Message.notification -> unit

    val request : t -> Jsonrpc.Message.request -> Jsonrpc.Response.t Fiber.t
  end

  val submit : _ t -> Batch.t -> unit Fiber.t
end
