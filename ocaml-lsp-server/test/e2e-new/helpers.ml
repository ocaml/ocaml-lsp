open Test.Import

let client_capabilities = ClientCapabilities.create ()

let uri = DocumentUri.of_path "test.ml"

let test ?extra_env text req =
  let handler =
    Client.Handler.make
      ~on_notification:(fun client _notification ->
        Client.state client;
        Fiber.return ())
      ()
  in
  Test.run ~handler ?extra_env (fun client ->
      let run_client () =
        Client.start
          client
          (InitializeParams.create ~capabilities:client_capabilities ())
      in
      let run () =
        let* (_ : InitializeResult.t) = Client.initialized client in
        let textDocument =
          TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
        in
        let* () =
          Client.notification
            client
            (TextDocumentDidOpen
               (DidOpenTextDocumentParams.create ~textDocument))
        in
        let* () = req client in
        let* () = Client.request client Shutdown in
        Client.stop client
      in
      Fiber.fork_and_join_unit run_client run)
