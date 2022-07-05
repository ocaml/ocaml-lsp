open Test.Import

let%expect_test "uri" =
  let notif_received = Fiber.Ivar.create () in
  let handler =
    let on_notification _ (notification : Lsp.Server_notification.t) =
      (match notification with
      | PublishDiagnostics d ->
        print_endline "client: received publish diagnostics notification";
        Printf.printf "uri received from the server: %s\n" (d.uri |> DocumentUri.to_string)
      | ShowMessage _
      | LogMessage _
      | TelemetryNotification _
      | CancelRequest _
      | WorkDoneProgress _
      | UnknownNotification _ -> ());
      let* () = Fiber.Ivar.fill notif_received () in
      Fiber.return ()
    in
    Client.Handler.make ~on_notification ()
  in
  ( Test.run ~handler @@ fun client ->
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let run =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let uri = DocumentUri.of_path "src/néw/Mödel + Other Thîngß/test.ml" in
      Printf.printf "uri sent to the server: %s\n" (DocumentUri.to_string uri);
      let textDocument =
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~text:"" ~version:0
      in
      let params = DidOpenTextDocumentParams.create ~textDocument in
      let* () = Client.notification client (TextDocumentDidOpen params) in
      Fiber.Ivar.read notif_received
    in
    Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client)
  );
  [%expect
    {|
      uri sent to the server: file:///src/néw/Mödel + Other Thîngß/test.ml
      client: received publish diagnostics notification
      uri received from the server: file:///src/néw/Mödel + Other Thîngß/test.ml |}]
