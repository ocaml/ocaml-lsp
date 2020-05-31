open! Import
open Jsonrpc

module Message : sig
  type ('request, 'notif) t =
    | Request of Id.t * 'request
    | Notification of 'notif

  val of_jsonrpc :
       (Request.t -> ('r, string) result)
    -> (Request.t -> ('n, string) result)
    -> Request.t
    -> (('r, 'n) t, string) result
end

module Io : sig
  type t

  val make : in_channel -> out_channel -> t

  val send : t -> packet -> unit Fiber.t

  val read : t -> packet option Fiber.t
end

module Stream_io : sig
  type t = packet Fiber_stream.In.t * packet Fiber_stream.Out.t

  val close : 'a * 'b Fiber_stream.Out.t -> unit Fiber.t

  val send : 'a * 'b Fiber_stream.Out.t -> 'b -> unit Fiber.t

  val recv : 'a Fiber_stream.In.t * 'b -> 'a option Fiber.t

  val make :
    Scheduler.t -> Io.t -> packet Fiber_stream.In.t * packet Fiber_stream.Out.t
end

module type S = sig
  type 'a out_request

  type out_notification

  type 'a in_request

  type in_notification

  module Handler : sig
    type 'self t

    type 'self on_request =
      { on_request :
          'a. 'self -> 'a in_request -> ('a, Response.Error.t) result Fiber.t
      }

    val make :
         ?on_request:'self on_request
      -> ?on_notification:('self -> in_notification -> unit Fiber.t)
      -> unit
      -> 'self t
  end

  type 'state t

  val make : 'state t Handler.t -> Stream_io.t -> 'state -> 'state t

  val stop : 'state t -> unit Fiber.t

  val request :
    _ t -> 'resp out_request -> ('resp, Response.Error.t) result Fiber.t

  val notification : _ t -> out_notification -> unit Fiber.t
end

module Client : sig
  open Types

  include
    S
      with type 'a out_request = 'a Client_request.t
       and type out_notification = Client_notification.t
       and type 'a in_request = 'a Server_request.t
       and type in_notification = Server_notification.t

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
