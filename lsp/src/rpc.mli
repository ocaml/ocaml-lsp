open! Import
open Jsonrpc

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

  type 'state t

  module Handler : sig
    type 'a session

    type 'state on_request =
      { on_request :
          'a.    'state session -> 'a in_request
          -> ('a * 'state, Response.Error.t) result Fiber.t
      }

    type 'state t

    val make :
         ?on_request:'state on_request
      -> ?on_notification:('state session -> in_notification -> 'state Fiber.t)
      -> unit
      -> 'state t
  end
  with type 'a session := 'a t

  val state : 'a t -> 'a

  val make : 'state Handler.t -> Stream_io.t -> 'state -> 'state t

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
