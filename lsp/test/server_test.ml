open Lsp
open! Import
open Lsp.Types

let scheduler = Scheduler.create ()

module Client = struct
  type state = { received_notification : unit Fiber.Ivar.t }

  let on_request (type a) _ (_ : a Server_request.t) :
      (_, Jsonrpc.Response.Error.t) result Fiber.t =
    Fiber.return
      (Error
         (Jsonrpc.Response.Error.make ~message:"not implemented"
            ~code:InternalError ()))

  let on_notification (client : state Client.t) n =
    let state = Client.state client in
    let req = Server_notification.to_jsonrpc n in
    Format.eprintf "client: received notification@.%s@.%!" req.method_;
    let (_ : unit Fiber.t) = Fiber.Ivar.fill state.received_notification () in
    Fiber.return state

  let handler =
    let on_request = { Client.Handler.on_request } in
    Client.Handler.make ~on_request ~on_notification ()

  let run in_ out =
    let initialize =
      let capabilities = Types.ClientCapabilities.create () in
      Types.InitializeParams.create ~capabilities ()
    in
    let client =
      let io = Rpc.Io.make in_ out in
      let stream_io = Rpc.Stream_io.make scheduler io in
      Client.make handler stream_io
        { received_notification = Fiber.Ivar.create () }
    in
    let running = Client.start client initialize in
    let open Fiber.O in
    let init () =
      Format.eprintf "client: waiting for initialization@.%!";
      let* (_ : Types.InitializeResult.t) = Client.initialized client in
      Format.eprintf "client: server initialized. sending request@.%!";
      let req =
        Client_request.ExecuteCommand
          (ExecuteCommandParams.create ~command:"foo" ())
      in
      let* resp = Client.request client req in
      Format.eprintf "client: sending request@.%!";
      match resp with
      | Error _ -> failwith "unexpected failure running command"
      | Ok json ->
        Format.eprintf
          "client: Successfully executed command with result:@.%s@."
          (Yojson.Safe.pretty_to_string json);
        let state = Client.state client in
        Format.eprintf
          "client: waiting to receive notification before shutdown @.%!";
        let* () = Fiber.Ivar.read state.received_notification in
        Format.eprintf "client: sending request to shutdown@.%!";
        Client.notification client Exit
    in
    let* () = Scheduler.detach ~name:"client init" scheduler init in
    running
end

module Server = struct
  module Server = Rpc.Server

  type state =
    | Started
    | Initialized

  let on_request =
    let on_request (type a) self (req : a Client_request.t) :
        (a * state, Jsonrpc.Response.Error.t) result Fiber.t =
      let state = Server.state self in
      match req with
      | Client_request.Initialize _ ->
        let capabilities = ServerCapabilities.create () in
        let result = InitializeResult.create ~capabilities () in
        Format.eprintf "server: initializing server@.";
        Format.eprintf "server: returning initialization result@.%!";
        Fiber.return (Ok (result, Initialized))
      | ExecuteCommand _ ->
        Format.eprintf "server: executing command@.%!";
        let result = `String "successful execution" in
        let open Fiber.O in
        let* () =
          Scheduler.detach ~name:"ShowMessage" scheduler (fun () ->
              Format.eprintf
                "server: sending message notification to client@.%!";
              let msg =
                ShowMessageParams.create ~type_:MessageType.Info ~message:"foo"
              in
              Server.notification self (Server_notification.ShowMessage msg))
        in
        Fiber.return (Ok (result, state))
      | _ ->
        Fiber.return
          (Error
             (Jsonrpc.Response.Error.make ~code:InternalError
                ~message:"not supported" ()))
    in
    { Server.Handler.on_request }

  let on_notification self _ =
    let state = Server.state self in
    Format.eprintf "server: Received notification@.%!";
    Fiber.return state

  let handler = Server.Handler.make ~on_request ~on_notification ()

  let run in_ out =
    let server =
      let io = Rpc.Io.make in_ out in
      let stream_io = Rpc.Stream_io.make scheduler io in
      Server.make handler stream_io Started
    in
    Server.start server
end

let pipe () =
  let in_, out = Unix.pipe () in
  (Unix.in_channel_of_descr in_, Unix.out_channel_of_descr out)

let () =
  (Lsp.Import.Log.level := fun _ -> true);
  let client_in, server_out = pipe () in
  let server_in, client_out = pipe () in
  let server () = Server.run server_in server_out in
  let client () = Client.run client_in client_out in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        let delay = 3.0 in
        Thread.delay delay;
        Format.eprintf "Test failed to terminate before %.2f seconds@." delay;
        Format.eprintf "----@.%a-----@." Scheduler.report scheduler;
        exit 1)
      ()
  in
  Scheduler.run scheduler (Fiber.fork_and_join_unit client server)
