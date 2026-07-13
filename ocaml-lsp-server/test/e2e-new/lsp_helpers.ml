open Test.Import

let change_config ~client params = Client.notification client (ChangeConfiguration params)

let open_document ~language_id ~client ~uri ~source =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:language_id ~version:0 ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let create_handler_with_diagnostics_callbacks ~got_diagnostics ~diagnostics_callback =
  Client.Handler.make ~on_notification:(fun _ -> function
    | PublishDiagnostics diagnostics ->
      let () = diagnostics_callback diagnostics in
      let* diag = Fiber.Ivar.peek got_diagnostics in
      (match diag with
       | Some _ -> Fiber.return ()
       | None -> Fiber.Ivar.fill got_diagnostics ())
    | _ -> Fiber.return ())
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
  let* _ = Client.initialized client in
  let* () =
    let settings = `Assoc [ "merlinDiagnostics", `Assoc [ "enable", `Bool true ] ] in
    Client.notification client (ChangeConfiguration { settings })
  in
  let uri = DocumentUri.of_path path in
  let* () = prep client in
  open_document ~language_id:"ocaml" ~client ~uri ~source
;;

let iter_lsp_response_internal
      ?(prep = fun _ -> Fiber.return ())
      ?(path = "foo.ml")
      ~language_id
      ~makeRequest
      ~source
      k
  =
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
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
  let request () =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let uri = DocumentUri.of_path path in
    let* () = prep client in
    let* () = open_document ~language_id ~client ~uri ~source in
    let* response =
      Fiber.collect_errors (fun () ->
        let request =
          let textDocument = TextDocumentIdentifier.create ~uri in
          makeRequest textDocument
        in
        Client.request client request)
    in
    k response
  in
  let shutdown () =
    let* () = Client.request client Shutdown in
    Client.notification client Exit
  in
  Fiber.fork_and_join_unit run_client (fun () -> Fiber.finalize request ~finally:shutdown)
;;

let iter_lsp_response ?prep ?path ~language_id ~makeRequest ~source k =
  iter_lsp_response_internal ?prep ?path ~language_id ~makeRequest ~source (function
    | Ok response ->
      k response;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors)
;;

let iter_lsp_response_result ?prep ?path ~language_id ~makeRequest ~source k =
  iter_lsp_response_internal ?prep ?path ~language_id ~makeRequest ~source (function
    | Ok response ->
      k (Ok response);
      Fiber.return ()
    | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
      ->
      k (Error error);
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors)
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
    create_handler_with_diagnostics_callbacks ~got_diagnostics ~diagnostics_callback ()
  in
  Test.run ~handler
  @@ fun client ->
  let run_client = create_client client in
  let open_document = open_document_with_client ~prep ~path ~source client in
  Fiber.fork_and_join_unit run_client (fun () ->
    open_document >>> Fiber.Ivar.read got_diagnostics >>> Client.stop client)
;;
