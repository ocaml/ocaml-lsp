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
  type packet =
    | Request of Request.t
    | Response of Response.t

  type t

  val make : in_channel -> out_channel -> t

  val send : t -> packet -> unit Fiber.t

  val read_request : t -> (Request.t, string) result Fiber.t

  val read_response : t -> (Response.t, string) result Fiber.t
end
