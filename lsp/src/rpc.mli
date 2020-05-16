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

  val read_request : t -> (Request.t, string) result Fiber.t

  val read_response : t -> (Response.t, string) result Fiber.t
end

type client = Client

type server = Server

type (_, 'resp) request =
  | Client : 'resp Client_request.t -> (client, 'resp) request
  | Server : 'resp Server_request.t -> (server, 'resp) request

type _ notification =
  | Client : Client_notification.t -> client notification
  | Server : Server_notification.t -> server notification

module Handler : sig
  type _ t

  val server :
       ?on_request:('a Client_request.t -> 'a Fiber.t)
    -> ?on_notification:(Client_notification.t -> unit)
    -> unit
    -> server t

  val client :
       ?on_request:('a Server_request.t -> 'a Fiber.t)
    -> ?on_notification:(Server_notification.t -> unit)
    -> unit
    -> client t
end

type _ t

val make : 'a Handler.t -> Io.t -> 'a t

val stop : _ t -> unit

val start_client :
  client t -> Types.InitializeParams.t -> Types.InitializeResult.t Fiber.t

val start_server : server t -> unit

val stopped : _ t -> unit Fiber.t

val initialized : _ t -> unit Fiber.t

val request : 'a t -> ('a, 'resp) request -> 'resp Fiber.t

val notification : 'a t -> 'a notification -> unit Fiber.t
