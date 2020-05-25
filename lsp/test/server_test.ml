open Lsp
open! Import

let scheduler = Scheduler.create ()

module Client = struct
  let on_request _ =
    Format.eprintf "fix me@.%!";
    failwith "not implemented"

  let on_notification _ = Format.eprintf "client: received notification@.%!"

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
    in
    let running = Client.start client initialize in
    let open Fiber.O in
    let* init =
      Fiber.fork (fun () ->
          let+ (_ : Types.InitializeResult.t) = Client.initialized client in
          Format.eprintf "client: initialized server@.%!")
    in
    Scheduler.detach scheduler (fun () -> Fiber.Future.wait init);
    running
end

module Server = struct
  open Lsp.Types
  module Server = Rpc.Server

  type state =
    | Started
    | Initialized

  let on_request state =
    let on_request (type a) (req : a Client_request.t) : a Fiber.t =
      match req with
      | Client_request.Initialize _ ->
        state := Initialized;
        let capabilities = ServerCapabilities.create () in
        let result = InitializeResult.create ~capabilities () in
        Format.eprintf "server: initializing server@.";
        Fiber.return result
      | _ -> failwith "not supported"
    in
    { Server.Handler.on_request }

  let on_notification _ = Format.eprintf "server: Received notification@.%!"

  let handler state =
    let on_request = on_request state in
    Server.Handler.make ~on_request ~on_notification ()

  let run in_ out =
    let state = ref Started in
    let server =
      let io = Rpc.Io.make in_ out in
      let stream_io = Rpc.Stream_io.make scheduler io in
      Server.make (handler state) stream_io
    in
    Server.start server
end

let pipe () =
  let in_, out = Unix.pipe () in
  (Unix.in_channel_of_descr in_, Unix.out_channel_of_descr out)

let () =
  let client_in, server_out = pipe () in
  let server_in, client_out = pipe () in
  let server () = Server.run server_in server_out in
  let client () = Client.run client_in client_out in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        let delay = 3.0 in
        Thread.delay delay;
        Format.eprintf "Test failed to terminate before %f seconds" delay;
        exit 1)
      ()
  in
  Scheduler.run scheduler (Fiber.fork_and_join_unit client server)
