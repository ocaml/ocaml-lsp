open Fiber.O
open Lsp
open Lsp.Types
open Lsp_fiber

module Test = struct
  module Client = struct
    let run
      ?(capabilities = ClientCapabilities.create ())
      ?on_request
      ?on_notification
      state
      (in_, out)
      =
      let initialize = InitializeParams.create ~capabilities () in
      let client =
        let stream_io = Lsp_fiber.Fiber_io.make in_ out in
        let handler = Client.Handler.make ?on_request ?on_notification () in
        Client.make handler stream_io state
      in
      client, Client.start client initialize
    ;;
  end

  module Server = struct
    let run ?on_request ?on_notification state (in_, out) =
      let server =
        let stream_io = Fiber_io.make in_ out in
        let handler = Server.Handler.make ?on_request ?on_notification () in
        Server.make handler stream_io state
      in
      server, Server.start server
    ;;
  end
end

let pipe () = Lev_fiber.Io.pipe ~cloexec:true ()

let test make_client make_server =
  Printexc.record_backtrace false;
  let run () =
    let* client_in, server_out = pipe () in
    let* server_in, client_out = pipe () in
    let server () = make_server (server_in, server_out) in
    let client () = make_client (client_in, client_out) in
    let+ () = Fiber.fork_and_join_unit server client in
    print_endline "Successful termination of test"
  in
  Lev_fiber.run run |> Lev_fiber.Error.ok_exn;
  print_endline "[TEST] finished"
;;

let json_pp = Yojson.Safe.pretty_print ~std:false

module End_to_end_client = struct
  let on_request (type a) _ (_ : a Server_request.t) =
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~message:"not implemented" ~code:InternalError ())
  ;;

  let on_notification (client : _ Client.t) n =
    let state = Client.state client in
    let received_notification = state in
    let req = Server_notification.to_jsonrpc n in
    Format.eprintf
      "client: received notification@.%a@.%!"
      json_pp
      (Jsonrpc.Notification.yojson_of_t req);
    let+ () = Fiber.Ivar.fill received_notification () in
    Format.eprintf "client: filled received_notification@.%!";
    state
  ;;

  let run io =
    let detached = Fiber.Pool.create () in
    let received_notification = Fiber.Ivar.create () in
    let client, running =
      let on_request = { Client.Handler.on_request } in
      Test.Client.run ~on_request ~on_notification received_notification io
    in
    let init () : unit Fiber.t =
      Format.eprintf "client: waiting for initialization@.%!";
      let* (_ : InitializeResult.t) = Client.initialized client in
      Format.eprintf "client: server initialized. sending request@.%!";
      let cancel = Fiber.Cancel.create () in
      let* () =
        Fiber.Pool.task detached ~f:(fun () ->
          Format.eprintf
            "client: waiting to receive notification before cancelling the request@.%!";
          let* () = Fiber.Ivar.read received_notification in
          Format.eprintf "client: received notification, cancelling the request@.%!";
          Fiber.Cancel.fire cancel)
      in
      let* res_cancel =
        let req_cancel =
          Client_request.ExecuteCommand
            (ExecuteCommandParams.create ~command:"cmd_cancel" ())
        in
        Format.eprintf "client: sending request cmd_cancel@.%!";
        Client.request_with_cancel client cancel req_cancel
      and* res_reply =
        let req_reply =
          Client_request.ExecuteCommand
            (ExecuteCommandParams.create ~command:"cmd_reply" ())
        in
        Format.eprintf "client: sending request cmd_reply@.%!";
        Client.request client req_reply
      in
      (match res_cancel with
       | `Cancelled -> Format.eprintf "client: req_cancel got cancelled@.%!"
       | `Ok _ -> assert false);
      Format.eprintf
        "client: Successfully executed req_reply with result:@.%a@."
        json_pp
        res_reply;
      Format.eprintf "client: sending request to shutdown@.%!";
      let* () = Fiber.Pool.stop detached in
      Client.notification client Exit
    in
    Fiber.fork_and_join_unit init (fun () ->
      Fiber.fork_and_join_unit (fun () -> running) (fun () -> Fiber.Pool.run detached))
  ;;
end

module End_to_end_server = struct
  type status =
    | Started
    | Initialized

  let on_request =
    let on_request (type a) self (req : a Client_request.t) : (a Rpc.Reply.t * _) Fiber.t =
      let state = Server.state self in
      let _status, detached = state in
      match req with
      | Client_request.Initialize _ ->
        let capabilities = ServerCapabilities.create () in
        let result = InitializeResult.create ~capabilities () in
        Format.eprintf "server: initializing server@.";
        Format.eprintf "server: returning initialization result@.%!";
        Fiber.return (Rpc.Reply.now result, (Initialized, detached))
      | Client_request.ExecuteCommand { command; _ } ->
        Format.eprintf "server: received command %s@.%!" command;
        let* () =
          match command with
          | "cmd_cancel" ->
            Fiber.Pool.task detached ~f:(fun () ->
              Format.eprintf "server: sending message notification to client@.%!";
              let msg =
                ShowMessageParams.create
                  ~type_:MessageType.Info
                  ~message:"notifying client"
              in
              Server.notification self (Server_notification.ShowMessage msg))
          | _ -> Fiber.return ()
        in
        let* () = Fiber.Pool.stop detached in
        let result = `String "successful execution" in
        let* cancel = Rpc.Server.cancel_token () in
        (match command with
         | "cmd_cancel" ->
           let+ () = Lev_fiber.Timer.sleepf 0.2 in
           ( Rpc.Reply.later (fun k ->
               let* cancel = Rpc.Server.cancel_token () in
               (* Make sure that we can access the cancel token in a Reply
                  response *)
               assert (Option.is_some cancel);
               k result)
           , state )
         | _ ->
           (* Make sure that we can access the cancel token in a Now response *)
           assert (Option.is_some cancel);
           Fiber.return (Rpc.Reply.now result, state))
      | _ ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make ~code:InternalError ~message:"not supported" ())
    in
    { Server.Handler.on_request }
  ;;

  let on_notification self _ =
    let state = Server.state self in
    Format.eprintf "server: Received notification@.%!";
    Fiber.return state
  ;;

  let run io =
    let detached = Fiber.Pool.create () in
    let _server, running =
      Test.Server.run ~on_request ~on_notification (Started, detached) io
    in
    Fiber.fork_and_join_unit (fun () -> running) (fun () -> Fiber.Pool.run detached)
  ;;
end

let%expect_test "end to end run of lsp tests" =
  test End_to_end_client.run End_to_end_server.run;
  [%expect
    {|
    client: waiting for initialization
    server: initializing server
    server: returning initialization result
    client: server initialized. sending request
    client: sending request cmd_cancel
    client: sending request cmd_reply
    client: waiting to receive notification before cancelling the request
    server: received command cmd_cancel
    server: sending message notification to client
    client: received notification
    {
      "params": { "message": "notifying client", "type": 3 },
      "method": "window/showMessage",
      "jsonrpc": "2.0"
    }
    client: filled received_notification
    client: received notification, cancelling the request
    server: received command cmd_reply
    client: req_cancel got cancelled
    client: Successfully executed req_reply with result:
    "successful execution"
    client: sending request to shutdown
    Successful termination of test
    [TEST] finished |}]
;;
