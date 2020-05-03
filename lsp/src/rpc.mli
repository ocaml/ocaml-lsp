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
