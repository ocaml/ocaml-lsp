open Import

module Message = struct
  type ('request, 'notif) t =
    | Request of Jsonrpc.Id.t * 'request
    | Notification of 'notif

  let of_jsonrpc req notif (packet : Jsonrpc.Request.t) =
    let open Result.O in
    match packet.id with
    | None ->
      let+ n = notif packet in
      Notification n
    | Some id ->
      let+ req = req packet in
      Request (id, req)
end
