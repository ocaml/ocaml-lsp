(** * This encodes LSP RPC state machine. *)

open! Import

module Reply : sig
  type 'resp t

  val now : 'r -> 'r t
  val later : (('r -> unit Fiber.t) -> unit Fiber.t) -> 'r t
end

module type S = sig
  type 'a out_request
  type out_notification
  type 'a in_request
  type in_notification
  type 'state t

  module Handler : sig
    type 'a session := 'a t

    type 'state on_request =
      { on_request : 'a. 'state session -> 'a in_request -> ('a Reply.t * 'state) Fiber.t
      }

    type 'state t

    val make
      :  ?on_request:'state on_request
      -> ?on_notification:('state session -> in_notification -> 'state Fiber.t)
      -> unit
      -> 'state t
  end

  val state : 'a t -> 'a
  val make : 'state Handler.t -> Fiber_io.t -> 'state -> 'state t
  val stop : 'state t -> unit Fiber.t
  val request : _ t -> 'resp out_request -> 'resp Fiber.t
  val notification : _ t -> out_notification -> unit Fiber.t

  (** only available inside requests *)
  val cancel_token : unit -> Fiber.Cancel.t option Fiber.t

  module Batch : sig
    type 'a session := 'a t
    type t

    val create : _ session -> t
    val notification : t -> out_notification -> unit

    type 'a response

    val await : 'a response -> 'a Fiber.t
    val request : t -> 'resp out_request -> 'resp response
    val submit : t -> unit Fiber.t
  end
end

module Client : sig
  open Types

  include
    S
    with type 'a out_request = 'a Client_request.t
     and type out_notification = Client_notification.t
     and type 'a in_request = 'a Server_request.t
     and type in_notification = Server_notification.t

  val request_with_cancel
    :  _ t
    -> Fiber.Cancel.t
    -> 'resp out_request
    -> [ `Ok of 'resp | `Cancelled ] Fiber.t

  val initialized : _ t -> InitializeResult.t Fiber.t
  val start : _ t -> InitializeParams.t -> unit Fiber.t
end

module Server : sig
  open Types

  include
    S
    with type 'a out_request = 'a Server_request.t
     and type out_notification = Server_notification.t
     and type 'a in_request = 'a Client_request.t
     and type in_notification = Client_notification.t

  val initialized : _ t -> InitializeParams.t Fiber.t
  val start : _ t -> unit Fiber.t
end
