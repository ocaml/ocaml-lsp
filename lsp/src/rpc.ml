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

module Io = struct
  let send oc json =
    let data = Yojson.Safe.to_string json in
    let content_length = String.length data in
    let header = Header.create ~content_length in
    Header.write header oc;
    output_string oc data;
    flush oc
end
