open Lsp
open! Import

module Client = struct
  let on_request _ _ =
    let code = Jsonrpc.Response.Error.Code.InternalError in
    let message = "Request not supported" in
    Fiber.return (Error (Jsonrpc.Response.Error.make ~code ~message ()))

  let on_notification _ _notif = Format.eprintf "Received notification@.%!"

  let handler = { Client.on_request; on_notification }

  let run in_ out =
    let initialize =
      let capabilities = Types.ClientCapabilities.create () in
      Types.InitializeParams.create ~capabilities ()
    in
    let client = Client.create handler in_ out initialize in
    let start () =
      let _start = Client.start client in
      let open Fiber.O in
      let+ (_ : Types.InitializeResult.t) = Client.initialized client in
      Format.eprintf "initialized"
    in
    Thread.create start ()
end

module Server = struct
  type state =
    | Started
    | Initialized

  let on_initialize _ s _ =
    assert (s = Started);
    let ir =
      let capabilities = Types.ServerCapabilities.create () in
      Types.InitializeResult.create ~capabilities ()
    in
    Format.eprintf "initialized server@.%!";
    Fiber.return (Ok (Initialized, ir))

  let on_request _ state _ _ =
    assert (state = Initialized);
    let code = Jsonrpc.Response.Error.Code.InternalError in
    let message = "Request not supported" in
    Fiber.return (Error (Jsonrpc.Response.Error.make ~code ~message ()))

  let on_notification _ state _notif =
    assert (state = Initialized);
    Format.eprintf "Received notification@.%!";
    Fiber.return (Ok state)

  let handler = { Server.on_initialize; on_request; on_notification }

  let run in_ out =
    let loop () = Server.start Started handler in_ out |> Fiber.run in
    Thread.create loop ()
end

let () =
  let client_in, server_out = Unix.pipe () in
  let server_in, client_out = Unix.pipe () in
  let server_thread =
    let in_ = Unix.in_channel_of_descr server_in in
    let out = Unix.out_channel_of_descr server_out in
    Server.run in_ out
  in
  let client_thread =
    let in_ = Unix.in_channel_of_descr client_in in
    let out = Unix.out_channel_of_descr client_out in
    Client.run in_ out
  in
  let (_ : Thread.t) =
    Thread.create
      (fun () ->
        Thread.delay 5.0;
        Format.eprintf "Test failed to terminate before 5 seconds";
        exit 1)
      ()
  in
  Thread.join server_thread;
  Thread.join client_thread
