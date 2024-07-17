open Test.Import

let open_document ~client ~uri ~source =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let iter_lsp_response
  ?(prep = fun _ -> Fiber.return ())
  ?(path = "foo.ml")
  ~makeRequest
  ~source
  k
  =
  let got_diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ ->
        function
        | PublishDiagnostics _ ->
          let* diag = Fiber.Ivar.peek got_diagnostics in
          (match diag with
           | Some _ -> Fiber.return ()
           | None -> Fiber.Ivar.fill got_diagnostics ())
        | _ -> Fiber.return ())
      ()
  in
  Test.run ~handler
  @@ fun client ->
  let run_client () =
    let capabilities =
      let window =
        let showDocument = ShowDocumentClientCapabilities.create ~support:true in
        WindowClientCapabilities.create ~showDocument ()
      in
      ClientCapabilities.create ~window ()
    in
    Client.start client (InitializeParams.create ~capabilities ())
  in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let uri = DocumentUri.of_path path in
    let* () = prep client in
    let* () = open_document ~client ~uri ~source in
    let+ resp =
      let request =
        let textDocument = TextDocumentIdentifier.create ~uri in
        makeRequest textDocument
      in
      Client.request client request
    in
    k resp
  in
  Fiber.fork_and_join_unit run_client (fun () ->
    run >>> Fiber.Ivar.read got_diagnostics >>> Client.stop client)
;;
