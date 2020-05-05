open Import
open Jsonrpc

module Message = struct
  type ('request, 'notif) t =
    | Request of Id.t * 'request
    | Notification of 'notif

  let of_jsonrpc req notif (packet : Request.t) =
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

  type packet =
    | Request of Request.t
    | Response of Response.t

  type t =
    { ic : in_channel
    ; oc : out_channel
    }

  let make ic oc =
    set_binary_mode_in ic true;
    set_binary_mode_out oc true;
    { ic; oc }

  let send { oc; ic = _ } (packet : packet) =
    let json =
      match packet with
      | Request r -> Request.yojson_of_t r
      | Response r -> Response.yojson_of_t r
    in
    log ~title:Logger.Title.LocalDebug "send: %a"
      (fun () -> Yojson.Safe.pretty_to_string ~std:false)
      json;
    let data = Yojson.Safe.to_string json in
    let content_length = String.length data in
    let header = Header.create ~content_length in
    Header.write header oc;
    output_string oc data;
    flush oc;
    Fiber.return ()

  let read { ic; oc = _ } =
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

  let read_request (t : t) =
    Fiber.return
      (let open Result.O in
      let* parsed = read t in
      match Jsonrpc.Request.t_of_yojson parsed with
      | r -> Ok r
      | exception _exn -> Error "Unexpected packet")

  let read_response (t : t) =
    Fiber.return
      (let open Result.O in
      let* parsed = read t in
      match Jsonrpc.Response.t_of_yojson parsed with
      | r -> Ok r
      | exception _exn -> Error "Unexpected packet")
end
