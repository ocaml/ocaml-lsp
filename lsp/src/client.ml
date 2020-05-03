open Import
open Types
open Rpc

type state =
  | Ready
  | Initialized
  | Closed

type handler =
  { on_request :
      'res.    t -> 'res Server_request.t
      -> ('res, Jsonrpc.Response.Error.t) result Fiber.t
  ; on_notification : t -> Server_notification.t -> unit
  }

and t =
  { ic : in_channel
  ; oc : out_channel
  ; mutable state : state
  ; initialize : InitializeParams.t
  ; initialized : InitializeResult.t Fiber.Ivar.t
  ; handler : handler
  }

let create handler ic oc initialize =
  let initialized = Fiber.Ivar.create () in
  let state = Ready in
  { ic; oc; state; initialize; handler; initialized }

let read_response (t : t) =
  let open Result.O in
  let* parsed = Io.read t.ic in
  match Jsonrpc.Response.t_of_yojson parsed with
  | r -> Ok r
  | exception _exn -> Error "Unexpected packet"

let read_request (t : t) =
  let open Result.O in
  let* parsed = Io.read t.ic in
  match Jsonrpc.Request.t_of_yojson parsed with
  | r -> Ok r
  | exception _exn -> Error "Unexpected packet"

let send_response t (response : Jsonrpc.Response.t) =
  let json = Jsonrpc.Response.yojson_of_t response in
  Fiber.return (Io.send t.oc json)

let read_message t =
  Result.bind (read_request t)
    ~f:
      (Message.of_jsonrpc Server_request.of_jsonrpc
         Server_notification.of_jsonrpc)

let req_id = ref 1

let send_request (type a) (t : t) (req : a Client_request.t) : a Fiber.t =
  let id = Either.Right !req_id in
  incr req_id;
  let () =
    Client_request.to_jsonrpc_request req ~id
    |> Jsonrpc.Request.yojson_of_t |> Io.send t.oc
  in
  match read_response t with
  | Error e -> failwith ("Invalid message" ^ e)
  | Ok m ->
    assert (m.id = id);
    let result =
      match m.result with
      | Error e -> Jsonrpc.Response.Error.raise e
      | Ok json -> Client_request.response_of_json req json
    in
    Fiber.return result

let start (t : t) =
  set_binary_mode_in t.ic true;
  set_binary_mode_out t.oc true;

  let on_initialized () =
    match read_message t with
    | Error _ ->
      (* TODO log this *)
      Fiber.return ()
    | Ok (Message.Notification notif) ->
      t.handler.on_notification t notif;
      Fiber.return ()
    | Ok (Message.Request (id, Server_request.E req)) -> (
      let handled =
        try t.handler.on_request t req
        with exn -> Fiber.return (Error (Jsonrpc.Response.Error.of_exn exn))
      in
      let open Fiber.O in
      let* handled = handled in
      match handled with
      | Ok result ->
        let yojson_result =
          match Server_request.yojson_of_result req result with
          | None -> `Null
          | Some res -> res
        in
        let response = Jsonrpc.Response.ok id yojson_result in
        send_response t response
      | Error e ->
        let response = Jsonrpc.Response.error id e in
        send_response t response )
  in

  let rec loop () =
    match t.state with
    | Closed -> Fiber.return ()
    | Initialized ->
      let open Fiber.O in
      let* () = on_initialized () in
      loop ()
    | Ready ->
      let open Fiber.O in
      let* response = send_request t (Client_request.Initialize t.initialize) in
      Logger.log_flush ();
      t.state <- Initialized;
      let* () = Fiber.Ivar.fill t.initialized response in
      loop ()
  in
  loop ()

let send_notification rpc notif =
  let response = Client_notification.to_jsonrpc_request notif in
  let json = Jsonrpc.Request.yojson_of_t response in
  Io.send rpc.oc json

let initialized (t : t) = Fiber.Ivar.read t.initialized

let stop t =
  t.state <- Closed;
  Fiber.return ()
