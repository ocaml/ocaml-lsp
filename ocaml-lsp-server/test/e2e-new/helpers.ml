open Test.Import

let client_capabilities = ClientCapabilities.create ()
let uri = DocumentUri.of_path "test.ml"

let test ?extra_env text req =
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run ~handler ?extra_env (fun client ->
    let run_client () =
      Client.start client (InitializeParams.create ~capabilities:client_capabilities ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = req client in
      let* () = Client.request client Shutdown in
      let* () = Fiber.Ivar.read diagnostics in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run)
;;
