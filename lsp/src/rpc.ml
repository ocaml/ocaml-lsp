open Import
open Gprotocol

type t =
  { ic : in_channel
  ; oc : out_channel
  ; fd : Unix.file_descr
  ; mutable state : state
  }

and state =
  | Ready
  | Initialized of ClientCapabilities.t
  | Closed

let { Logger.log } = Logger.for_section "lsp"

let send rpc json =
  log ~title:Logger.Title.LocalDebug "send: %a"
    (fun () -> Yojson.Safe.pretty_to_string ~std:false)
    json;
  let data = Yojson.Safe.to_string json in
  let content_length = String.length data in
  let header = Header.create ~content_length in
  Header.write header rpc.oc;
  output_string rpc.oc data;
  flush rpc.oc

let read rpc =
  let open Result.O in
  let read_content rpc =
    Thread.wait_read rpc.fd;
    let header = Header.read rpc.ic in
    let len = Header.content_length header in
    let buffer = Bytes.create len in
    let rec read_loop read =
      if read < len then
        let n = input rpc.ic buffer read (len - read) in
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

  read_content rpc >>= parse_json >>= fun parsed ->
  match Jsonrpc.Request.t_of_yojson parsed with
  | r -> Ok r
  | exception _exn -> Error "Unexpected packet"

let send_response rpc (response : Jsonrpc.Response.t) =
  let json = Jsonrpc.Response.yojson_of_t response in
  send rpc json

let send_notification rpc notif =
  let response = Server_notification.to_jsonrpc_request notif in
  let json = Jsonrpc.Request.yojson_of_t response in
  send rpc json

module Message = struct
  type t =
    | Request of Jsonrpc.Id.t * Client_request.packed
    | Client_notification of Client_notification.t

  let of_jsonrpc (packet : Jsonrpc.Request.t) =
    let open Result.O in
    match packet.id with
    | None ->
      Client_notification.of_jsonrpc packet >>| fun cn -> Client_notification cn
    | Some id -> Client_request.of_jsonrpc packet >>| fun r -> Request (id, r)
end

open Gprotocol

type 'state handler =
  { on_initialize :
         t
      -> 'state
      -> InitializeParams.t
      -> ('state * InitializeResult.t, string) result
  ; on_request :
      'res.    t -> 'state -> ClientCapabilities.t -> 'res Client_request.t
      -> ('state * 'res, Jsonrpc.Response.Error.t) result
  ; on_notification :
      t -> 'state -> Client_notification.t -> ('state, string) result
  }

let start init_state handler ic oc =
  let open Result.O in
  let read_message rpc = read rpc >>= Message.of_jsonrpc in

  let handle_message prev_state f =
    let start = Unix.gettimeofday () in
    let next_state = f () in
    let ellapsed = (Unix.gettimeofday () -. start) /. 1000.0 in
    log ~title:Logger.Title.LocalDebug "time elapsed processing message: %fs"
      ellapsed;
    match next_state with
    | Ok next_state -> next_state
    | Error msg ->
      log ~title:Logger.Title.Error "%s" msg;
      prev_state
  in

  let rec loop rpc state =
    match rpc.state with
    | Closed -> ()
    | Ready ->
      let next_state =
        handle_message state (fun () ->
            let open Client_request in
            read_message rpc >>= function
            | Message.Request (id, E (Initialize params)) ->
              handler.on_initialize rpc state params
              >>= fun (next_state, result) ->
              let json = InitializeResult.yojson_of_t result in
              let response = Jsonrpc.Response.ok id json in
              rpc.state <- Initialized params.capabilities;
              send_response rpc response;
              Ok next_state
            | Message.Client_notification Exit ->
              rpc.state <- Closed;
              Ok state
            | Message.Client_notification _ ->
              (* we drop all notifications per protocol before we initialized *)
              Ok state
            | Message.Request (id, _) ->
              (* we response with -32002 per protocol before we initialized *)
              let response =
                let error =
                  Jsonrpc.Response.Error.make ~code:ServerNotInitialized
                    ~message:"not initialized" ()
                in
                Jsonrpc.Response.error id error
              in
              send_response rpc response;
              Ok state)
      in
      Logger.log_flush ();
      loop rpc next_state
    | Initialized client_capabilities ->
      let next_state =
        handle_message state (fun () ->
            let open Client_request in
            read_message rpc >>= function
            | Message.Request (_id, E (Initialize _)) ->
              Error "received another initialize request"
            | Message.Client_notification (Exit as notif) ->
              rpc.state <- Closed;
              handler.on_notification rpc state notif
            | Message.Client_notification notif -> (
              try handler.on_notification rpc state notif
              with exn -> Error (Printexc.to_string exn) )
            | Message.Request (id, E req) -> (
              let handled =
                try handler.on_request rpc state client_capabilities req
                with exn -> Error (Jsonrpc.Response.Error.of_exn exn)
              in
              match handled with
              | Ok (next_state, result) ->
                let yojson_result =
                  match Client_request.yojson_of_result req result with
                  | None -> `Null
                  | Some res -> res
                in
                let response = Jsonrpc.Response.ok id yojson_result in
                send_response rpc response;
                Ok next_state
              | Error e ->
                let response = Jsonrpc.Response.error id e in
                send_response rpc response;
                Error e.message ))
      in
      Logger.log_flush ();
      loop rpc next_state
  in

  set_binary_mode_in ic true;
  set_binary_mode_out oc true;
  let fd = Unix.descr_of_in_channel stdin in
  let rpc = { ic; oc; fd; state = Ready } in
  loop rpc init_state

let stop (rpc : t) = rpc.state <- Closed
