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
  { rpc : Io.t
  ; mutable state : state
  ; initialize : InitializeParams.t
  ; initialized : InitializeResult.t Fiber.Ivar.t
  ; handler : handler
  }

let create handler ic oc initialize =
  let initialized = Fiber.Ivar.create () in
  let state = Ready in
  let rpc = Io.make ic oc in
  { rpc; state; initialize; handler; initialized }

let read_message t =
  let open Fiber.O in
  let+ req = Io.read_request t in
  Result.bind req
    ~f:
      (Message.of_jsonrpc Server_request.of_jsonrpc
         Server_notification.of_jsonrpc)

let req_id = ref 1

let send_request (type a) (t : t) (req : a Client_request.t) : a Fiber.t =
  let id = Either.Right !req_id in
  incr req_id;
  let open Fiber.O in
  let* () =
    Io.send t.rpc (Request (Client_request.to_jsonrpc_request req ~id))
  in
  let+ response = Io.read_response t.rpc in
  match response with
  | Error e -> failwith ("Invalid message" ^ e)
  | Ok m -> (
    assert (m.id = id);
    match m.result with
    | Error e -> Jsonrpc.Response.Error.raise e
    | Ok json -> Client_request.response_of_json req json )

let start (t : t) =
  let on_initialized () =
    let open Fiber.O in
    let* message = read_message t.rpc in
    match message with
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
        Io.send t.rpc (Response response)
      | Error e ->
        let response = Jsonrpc.Response.error id e in
        Io.send t.rpc (Response response) )
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
  let req = Client_notification.to_jsonrpc_request notif in
  let (_ : unit Fiber.t) = Io.send rpc.rpc (Request req) in
  ()

let initialized (t : t) = Fiber.Ivar.read t.initialized

let stop t =
  t.state <- Closed;
  Fiber.return ()
