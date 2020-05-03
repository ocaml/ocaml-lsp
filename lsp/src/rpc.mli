open Import

module Message : sig
  type ('request, 'notif) t =
    | Request of Jsonrpc.Id.t * 'request
    | Notification of 'notif

  val of_jsonrpc :
       (Jsonrpc.Request.t -> ('r, string) result)
    -> (Jsonrpc.Request.t -> ('n, string) result)
    -> Jsonrpc.Request.t
    -> (('r, 'n) t, string) result
end

module Io : sig
  val send : out_channel -> Json.t -> unit

  val read : in_channel -> (Json.t, string) result
end
