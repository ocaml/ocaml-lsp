open Fiber.O
open Lsp
open! Import
open Fiber_unix
open Lsp.Types
open Lsp_fiber

module Test = struct
  module Client = struct
    let run ?(capabilities = ClientCapabilities.create ()) ?on_request
        ?on_notification state (in_, out) =
      let initialize = InitializeParams.create ~capabilities () in
      let+ client =
        let io = Io.make in_ out in
        let+ stream_io = Lsp_fiber.Fiber_io.make io in
        let handler = Client.Handler.make ?on_request ?on_notification () in
        Client.make handler stream_io state
      in
      (client, Client.start client initialize)
  end

  module Server = struct
    let run ?on_request ?on_notification state (in_, out) =
      let+ server =
        let io = Io.make in_ out in
        let+ stream_io = Fiber_io.make io in
        let handler = Server.Handler.make ?on_request ?on_notification () in
        Server.make handler stream_io state
      in
      (server, Server.start server)
  end
end

let pipe () =
  let in_, out = Unix.pipe () in
  (Unix.in_channel_of_descr in_, Unix.out_channel_of_descr out)

let test make_client make_server =
  Printexc.record_backtrace false;
  let client_in, server_out = pipe () in
  let server_in, client_out = pipe () in
  let server () = make_server (server_in, server_out) in
  let client () = make_client (client_in, client_out) in
  let run () =
    let delay = 3.0 in
    let* timer = Scheduler.create_timer ~delay in
    Fiber.fork_and_join_unit
      (fun () ->
        let+ res = Scheduler.schedule timer (fun () -> Scheduler.abort ()) in
        match res with
        | Error `Cancelled -> ()
        | Ok () ->
          Printf.eprintf "Test failed to terminate inside %0.2f seconds" delay)
      (fun () ->
        let* () = Fiber.fork_and_join_unit server client in
        print_endline "Successful termination of test";
        Scheduler.cancel_timer timer)
  in
  Scheduler.run (run ());
  print_endline "[TEST] finished"

module End_to_end_client = struct
  let on_request (type a) _ (_ : a Server_request.t) =
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~message:"not implemented"
         ~code:InternalError ())

  let on_notification (client : _ Client.t) n =
    let open Fiber.O in
    let state = Client.state client in
    let received_notification = state in
    let req = Server_notification.to_jsonrpc n in
    Format.eprintf "client: received notification@.%s@.%!" req.method_;
    let+ () = Fiber.Ivar.fill received_notification () in
    Format.eprintf "client: filled received_notification@.%!";
    state

  let run io =
    let received_notification = Fiber.Ivar.create () in
    let* client, running =
      let on_request = { Client.Handler.on_request } in
      Test.Client.run ~on_request ~on_notification received_notification io
    in
    let open Fiber.O in
    let init () : unit Fiber.t =
      Format.eprintf "client: waiting for initialization@.%!";
      let* (_ : InitializeResult.t) = Client.initialized client in
      Format.eprintf "client: server initialized. sending request@.%!";
      let req =
        Client_request.ExecuteCommand
          (ExecuteCommandParams.create ~command:"foo" ())
      in
      Format.eprintf "client: sending request@.%!";
      let* json = Client.request client req in
      Format.eprintf "client: Successfully executed command with result:@.%s@."
        (Json.to_string json);
      Format.eprintf
        "client: waiting to receive notification before shutdown @.%!";
      let* () = Fiber.Ivar.read received_notification in
      Format.eprintf "client: sending request to shutdown@.%!";
      Client.notification client Exit
    in
    Fiber.fork_and_join_unit init (fun () -> running)
end

module End_to_end_server = struct
  module Server = Rpc.Server

  type status =
    | Started
    | Initialized

  let on_request =
    let on_request (type a) self (req : a Client_request.t) :
        (a Rpc.Reply.t * _) Fiber.t =
      let state = Server.state self in
      let _status, detached = state in
      match req with
      | Client_request.Initialize _ ->
        let capabilities = ServerCapabilities.create () in
        let result = InitializeResult.create ~capabilities () in
        Format.eprintf "server: initializing server@.";
        Format.eprintf "server: returning initialization result@.%!";
        Fiber.return (Rpc.Reply.now result, (Initialized, detached))
      | Client_request.ExecuteCommand _ ->
        Format.eprintf "server: executing command@.%!";
        let result = `String "successful execution" in
        let open Fiber.O in
        let* () =
          let* timer = Scheduler.create_timer ~delay:0.5 in
          Fiber.Pool.task detached ~f:(fun () ->
              Format.eprintf
                "server: sending message notification to client@.%!";
              let msg =
                ShowMessageParams.create ~type_:MessageType.Info ~message:"foo"
              in
              let rec loop n res : unit Fiber.t =
                if n = 0 then
                  let+ res = res in
                  let () =
                    match res with
                    | Ok () -> Format.eprintf "server: %d ran@.%!" n
                    | Error `Cancelled ->
                      Format.eprintf "server: %d cancellation@.%!" n
                  in
                  ()
                else
                  let res =
                    Format.eprintf "server: scheduling show message@.%!";
                    Scheduler.schedule timer (fun () ->
                        Format.eprintf
                          "server: sending show message notification@.%!";
                        Server.notification self
                          (Server_notification.ShowMessage msg))
                  in
                  loop (n - 1) res
              in
              loop 2 (Fiber.return (Ok ())))
        in
        let+ () = Fiber.Pool.stop detached in
        (Rpc.Reply.now result, state)
      | _ ->
        Jsonrpc.Response.Error.raise
          (Jsonrpc.Response.Error.make ~code:InternalError
             ~message:"not supported" ())
    in
    { Server.Handler.on_request }

  let on_notification self _ =
    let state = Server.state self in
    Format.eprintf "server: Received notification@.%!";
    Fiber.return state

  let run io =
    let detached = Fiber.Pool.create () in
    let* _server, running =
      Test.Server.run ~on_request ~on_notification (Started, detached) io
    in
    Fiber.fork_and_join_unit
      (fun () -> running)
      (fun () -> Fiber.Pool.run detached)
end

let%expect_test "ent to end run of lsp tests" =
  test End_to_end_client.run End_to_end_server.run;
  [%expect
    {|
  client: waiting for initialization
  server: initializing server
  server: returning initialization result
  client: server initialized. sending request
  client: sending request
  server: executing command
  server: sending message notification to client
  server: scheduling show message
  server: scheduling show message
  client: Successfully executed command with result:
  "successful execution"
  client: waiting to receive notification before shutdown
  server: sending show message notification
  server: 0 ran
  client: received notification
  window/showMessage
  client: filled received_notification
  client: sending request to shutdown
  Successful termination of test
  [TEST] finished |}]
