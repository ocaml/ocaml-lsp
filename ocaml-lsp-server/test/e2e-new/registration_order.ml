open Test.Import

let capabilities =
  let synchronization =
    TextDocumentSyncClientCapabilities.create ~dynamicRegistration:true ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~synchronization () in
  ClientCapabilities.create ~textDocument ()
;;

let%expect_test "dynamic registration arrives before initialized" =
  let registration_received = Fiber.Ivar.create () in
  let on_request
        (type resp state)
        (client : state Client.t)
        (request : resp Lsp.Server_request.t)
    : (resp Lsp_fiber.Rpc.Reply.t * state) Fiber.t
    =
    match request with
    | ClientRegisterCapability params ->
      let* () = Fiber.Ivar.fill registration_received params in
      Fiber.return (Lsp_fiber.Rpc.Reply.now (), Client.state client)
    | _ -> assert false
  in
  let handler =
    Client.Handler.make
      ~on_request:{ Client.Handler.on_request }
      ~on_notification:(fun _ _ -> Fiber.return ())
      ()
  in
  (Test.run ~handler
   @@ fun client ->
   let run_client () = Test.start_client ~capabilities client in
   let reproduce () =
     let* (_ : InitializeResult.t) = Client.initialized client in
     print_endline "received initialize response";
     let* registration = Fiber.Ivar.read registration_received in
     print_endline "received client/registerCapability before sending initialized:";
     RegistrationParams.yojson_of_t registration |> Test.print_result;
     print_endline "sending initialized";
     let* () = Client.notification client Initialized in
     Test.shutdown_client client
   in
   Fiber.fork_and_join_unit run_client reproduce);
  [%expect
    {|
    received initialize response
    received client/registerCapability before sending initialized:
    {
      "registrations": [
        {
          "id": "ocamllsp-cram-dune-files/textDocument/didOpen",
          "method": "textDocument/didOpen",
          "registerOptions": {
            "documentSelector": [
              { "language": "cram", "scheme": null, "pattern": null },
              { "language": "dune", "scheme": null, "pattern": null },
              { "language": "dune-project", "scheme": null, "pattern": null },
              { "language": "dune-workspace", "scheme": null, "pattern": null }
            ]
          }
        },
        {
          "id": "ocamllsp-cram-dune-files/textDocument/didClose",
          "method": "textDocument/didClose",
          "registerOptions": {
            "documentSelector": [
              { "language": "cram", "scheme": null, "pattern": null },
              { "language": "dune", "scheme": null, "pattern": null },
              { "language": "dune-project", "scheme": null, "pattern": null },
              { "language": "dune-workspace", "scheme": null, "pattern": null }
            ]
          }
        }
      ]
    }
    sending initialized
    |}]
;;
