open Test.Import

let open_document ~client ~uri ~source =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let create_handler_with_diagnostics_callback ~got_diagnostics ~diagnostics_callback =
  (* Calls [diagnostics_callback] and fills [got_diagnostics] when receiving diagnostics. *)
  Client.Handler.make
    ~on_notification:(fun _ -> function
      | PublishDiagnostics diagnostics ->
        diagnostics_callback diagnostics;
        let* diag = Fiber.Ivar.peek got_diagnostics in
        (match diag with
         | Some _ -> Fiber.return ()
         | None -> Fiber.Ivar.fill got_diagnostics ())
      | _ -> Fiber.return ())
    ()
;;

let create_client client =
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
  run_client
;;

let open_document_with_client ~prep ~path ~source client =
  let* (_ : InitializeResult.t) = Client.initialized client in
  let* () =
    let settings = `Assoc [ "merlinDiagnostics", `Assoc [ "enable", `Bool true ] ] in
    Client.notification client (ChangeConfiguration { settings })
  in
  let uri = DocumentUri.of_path path in
  let* () = prep client in
  open_document ~client ~uri ~source
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
    create_handler_with_diagnostics_callback
      ~got_diagnostics
      ~diagnostics_callback:(fun _ -> ())
  in
  Test.run ~handler
  @@ fun client ->
  let run_client = create_client client in
  let run =
    let* () = open_document_with_client ~prep ~path ~source client in
    let+ resp =
      let uri = DocumentUri.of_path path in
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

let open_document_with_diagnostics_callback
  ?(prep = fun _ -> Fiber.return ())
  ?(path = "foo.ml")
  ~source
  ~diagnostics_callback
  ()
  =
  let got_diagnostics = Fiber.Ivar.create () in
  let handler =
    create_handler_with_diagnostics_callback ~got_diagnostics ~diagnostics_callback
  in
  Test.run ~handler
  @@ fun client ->
  let run_client = create_client client in
  let open_document = open_document_with_client ~prep ~path ~source client in
  Fiber.fork_and_join_unit run_client (fun () ->
    open_document >>> Fiber.Ivar.read got_diagnostics >>> Client.stop client)
;;
