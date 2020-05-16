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

type client = Client

type server = Server

type (_, 'resp) request =
  | Client : 'resp Client_request.t -> (client, 'resp) request
  | Server : 'resp Server_request.t -> (server, 'resp) request

type _ notification =
  | Client : Client_notification.t -> client notification
  | Server : Server_notification.t -> server notification

module Handler = struct
  type 'a on_request = E : (('a, 'r) request -> 'r Fiber.t) -> 'a on_request

  type 'a gen =
    { on_request : 'a on_request
    ; on_notification : 'a notification -> unit
    }

  (* A server has a [client handler] A client has a [server handler] *)
  type _ t =
    | Client : server gen -> client t
    | Server : client gen -> server t

  let on_notification_default _ =
    (* TODO we can also just log it *)
    ()

  let client ?on_request ?(on_notification = on_notification_default) () =
    let on_request : server on_request =
      let f =
        match on_request with
        | Some s -> s
        | None -> assert false
      in
      E (fun (Server x) -> f x)
    in
    let on_notification : server notification -> unit =
     fun (Server x) -> on_notification x
    in
    Client { on_request; on_notification }

  let server ?on_request ?(on_notification = on_notification_default) () =
    let on_request : client on_request =
      let f =
        match on_request with
        | Some s -> s
        | None -> assert false
      in
      E (fun (Client x) -> f x)
    in
    let on_notification : client notification -> unit =
     fun (Client x) -> on_notification x
    in
    Server { on_request; on_notification }
end

module State = struct
  type t =
    | Waiting_for_init
    | Running
    | Closed
end

type 'h t =
  { handler : 'h Handler.t
  ; io : Io.t
  ; mutable state : State.t
  }

let make handler io = { handler; io; state = Waiting_for_init }

let req_id = ref 1

let request (type a r) (t : a t) (req : (a, r) request) : r Fiber.t =
  let id = Either.Right !req_id in
  let jsonrpc_request =
    incr req_id;
    match req with
    | Client r -> Client_request.to_jsonrpc_request r ~id
    | Server r -> Server_request.to_jsonrpc_request r ~id
  in
  let open Fiber.O in
  let* () = Io.send t.io (Request jsonrpc_request) in
  let* resp = Io.read_response t.io in
  match resp with
  | Error e -> failwith e
  | Ok r -> (
    assert (r.id = id);
    match r.result with
    | Error e -> Jsonrpc.Response.Error.raise e
    | Ok (json : Json.t) ->
      Fiber.return
        ( match req with
        | Client r -> Client_request.response_of_json r json
        | Server r -> Server_request.response_of_json r json ) )

let notification (type a) (t : a t) (n : a notification) : unit Fiber.t =
  let jsonrpc_request =
    match n with
    | Client r -> Client_notification.to_jsonrpc_request r
    | Server r -> Server_notification.to_jsonrpc_request r
  in
  Io.send t.io (Request jsonrpc_request)

let start_loop _ = ()

let start_client (t : client t) (p : Types.InitializeParams.t) =
  assert (t.state = Waiting_for_init);
  let open Fiber.O in
  let+ resp = request t (Client (Client_request.Initialize p)) in
  t.state <- Running;
  start_loop t;
  resp

let start_server t = start_loop t

let initialized _ = assert false

let stop t =
  t.state <- Closed;
  assert false

let stopped _ = assert false
