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
  let { Logger.log } = Logger.for_section "lsp_io"

  let send oc json =
    log ~title:Logger.Title.LocalDebug "send: %a"
      (fun () -> Yojson.Safe.pretty_to_string ~std:false)
      json;
    let data = Yojson.Safe.to_string json in
    let content_length = String.length data in
    let header = Header.create ~content_length in
    Header.write header oc;
    output_string oc data;
    flush oc

  let read ic =
    let read_content () =
      let header = Header.read ic in
      let len = Header.content_length header in
      let buffer = Bytes.create len in
      let rec read_loop read =
        if read < len then
          let n = input ic buffer read (len - read) in
          read_loop (read + n)
      in
      let () = read_loop 0 in
      Ok (Bytes.to_string buffer)
    in

    let parse_json content =
      match Yojson.Safe.from_string content with
      | json ->
        log ~title:Logger.Title.LocalDebug "recv: %a"
          (fun () -> Yojson.Safe.pretty_to_string ~std:false)
          json;
        Ok json
      | exception Yojson.Json_error msg ->
        Result.errorf "error parsing json: %s" msg
    in

    let open Result.O in
    read_content () >>= parse_json
end
