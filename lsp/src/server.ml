open Import
open Types
open Rpc

type state =
  | Ready
  | Initialized of ClientCapabilities.t
  | Closed

type t =
  { rpc : Rpc.Io.t
  ; mutable state : state
  }

let { Logger.log } = Logger.for_section "lsp"

let send_notification (t : t) notif =
  let request = Server_notification.to_jsonrpc_request notif in
  let (_ : unit Fiber.t) = Io.send t.rpc (Request request) in
  ()

open Types

type 'state handler =
  { on_initialize :
         t
      -> 'state
      -> InitializeParams.t
      -> ('state * InitializeResult.t, string) Fiber.Result.t
  ; on_request :
      'res.    t -> 'state -> ClientCapabilities.t -> 'res Client_request.t
      -> ('state * 'res, Jsonrpc.Response.Error.t) result Fiber.t
  ; on_notification :
      t -> 'state -> Client_notification.t -> ('state, string) result Fiber.t
  }

let handle_message prev_state f =
  let open Fiber.O in
  let start = Unix.gettimeofday () in
  let+ next_state = f () in
  let ellapsed = (Unix.gettimeofday () -. start) /. 1000.0 in
  log ~title:Logger.Title.LocalDebug "time elapsed processing message: %fs"
    ellapsed;
  match next_state with
  | Ok next_state -> next_state
  | Error msg ->
    log ~title:Logger.Title.Error "%s" msg;
    prev_state

let read_message t =
  let open Fiber.O in
  let+ req = Io.read_request t.rpc in
  Result.bind req
    ~f:
      (Message.of_jsonrpc Client_request.of_jsonrpc
         Client_notification.of_jsonrpc)

let on_ready handler rpc state =
  let open Fiber.Result.O in
  let* msg = read_message rpc in
  let open Client_request in
  match msg with
  | Message.Request (id, E (Initialize params)) ->
    let* next_state, result = handler.on_initialize rpc state params in
    let json = InitializeResult.yojson_of_t result in
    let response = Jsonrpc.Response.ok id json in
    rpc.state <- Initialized params.capabilities;
    let+ () = Fiber.Result.lift (Io.send rpc.rpc (Response response)) in
    next_state
  | Message.Notification Exit ->
    rpc.state <- Closed;
    Fiber.Result.return state
  | Message.Notification _ ->
    (* we drop all notifications per protocol before we initialized *)
    Fiber.Result.return state
  | Message.Request (id, _) ->
    (* we response with -32002 per protocol before we initialized *)
    let response =
      let error =
        Jsonrpc.Response.Error.make ~code:ServerNotInitialized
          ~message:"not initialized" ()
      in
      Jsonrpc.Response.error id error
    in
    let open Fiber.O in
    let+ () = Io.send rpc.rpc (Response response) in
    Ok state

let on_initialized handler client_capabilities rpc state =
  let open Fiber.Result.O in
  let* msg = read_message rpc in
  let open Client_request in
  match msg with
  | Message.Request (_id, E (Initialize _)) ->
    Fiber.return (Error "received another initialize request")
  | Message.Notification (Exit as notif) ->
    rpc.state <- Closed;
    handler.on_notification rpc state notif
  | Message.Notification notif -> (
    try handler.on_notification rpc state notif
    with exn -> Fiber.return (Error (Printexc.to_string exn)) )
  | Message.Request (id, E req) -> (
    let handled =
      try handler.on_request rpc state client_capabilities req
      with exn -> Fiber.return (Error (Jsonrpc.Response.Error.of_exn exn))
    in
    let open Fiber.O in
    let* handled = handled in
    match handled with
    | Ok (next_state, result) ->
      let yojson_result =
        match Client_request.yojson_of_result req result with
        | None -> `Null
        | Some res -> res
      in
      let response = Jsonrpc.Response.ok id yojson_result in
      let+ () = Io.send rpc.rpc (Response response) in
      Ok next_state
    | Error e ->
      let response = Jsonrpc.Response.error id e in
      let+ () = Io.send rpc.rpc (Response response) in
      Error e.message )

let start init_state handler ic oc =
  let rec loop rpc state : unit Fiber.t =
    match rpc.state with
    | Closed -> Fiber.return ()
    | Ready ->
      let open Fiber.O in
      let* next_state =
        handle_message state (fun () -> on_ready handler rpc state)
      in
      Logger.log_flush ();
      loop rpc next_state
    | Initialized client_capabilities ->
      let open Fiber.O in
      let* next_state =
        handle_message state (fun () ->
            on_initialized handler client_capabilities rpc state)
      in
      Logger.log_flush ();
      loop rpc next_state
  in

  let rpc =
    let rpc = Rpc.Io.make ic oc in
    { rpc; state = Ready }
  in
  loop rpc init_state

let stop (rpc : t) = rpc.state <- Closed
