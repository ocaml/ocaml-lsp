open Test.Import

let%expect_test "show an open document's text" =
  let diagnostics = Fiber.Mvar.create () in
  let handler =
    let on_request (type r) _ (request : r Lsp.Server_request.t)
      : (r Lsp_fiber.Rpc.Reply.t * unit) Fiber.t
      =
      match request with
      | ShowDocumentRequest params ->
        let path = Uri.to_path params.uri in
        let json =
          ShowDocumentParams.yojson_of_t { params with uri = Uri.of_path "<redacted>.ml" }
        in
        Test.print_result json;
        print_endline (Fs_io.read_file path |> Result.ok_exn);
        Sys.remove path;
        let response = ShowDocumentResult.create ~success:true in
        Fiber.return (Lsp_fiber.Rpc.Reply.now response, ())
      | _ -> assert false
    in
    Client.Handler.make
      ~on_request:{ Client.Handler.on_request }
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params -> Fiber.Mvar.write diagnostics params
         | _ -> Fiber.return ())
      ()
  in
  let capabilities =
    let showDocument = ShowDocumentClientCapabilities.create ~support:true in
    let window = WindowClientCapabilities.create ~showDocument () in
    ClientCapabilities.create ~window ()
  in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let uri = DocumentUri.of_path "document-text.ml" in
   let source = "let answer = 0\n" in
   let* () = Test.open_document ~client ~uri ~source () in
   let* (_ : PublishDiagnosticsParams.t) = Fiber.Mvar.read diagnostics in
   let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version:1 in
   let contentChanges =
     [ `TextDocumentContentChangeWholeDocument
         (TextDocumentContentChangeWholeDocument.create ~text:"let answer = 42\n")
     ]
   in
   let* () =
     Client.notification
       client
       (TextDocumentDidChange
          (DidChangeTextDocumentParams.create ~textDocument ~contentChanges))
   in
   let* (_ : PublishDiagnosticsParams.t) = Fiber.Mvar.read diagnostics in
   let params =
     ExecuteCommandParams.create
       ~command:"ocamllsp/show-document-text"
       ~arguments:[ DocumentUri.yojson_of_t uri ]
       ()
   in
   let* response = Client.request client (ExecuteCommand params) in
   Test.print_result response;
   Client.stop client);
  [%expect
    {|
    { "takeFocus": true, "uri": "file:///%3Credacted%3E.ml" }
    let answer = 42

    null |}]
;;
