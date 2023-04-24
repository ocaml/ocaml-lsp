open Test.Import

let%expect_test "it should allow double opening the same document" =
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:
        (fun _ -> function
          | PublishDiagnostics _ -> Fiber.Ivar.fill diagnostics ()
          | _ -> Fiber.return ())
      ()
  in
  ( Test.run ~handler @@ fun client ->
    let run_client () =
      let capabilities =
        let window =
          let showDocument =
            ShowDocumentClientCapabilities.create ~support:true
          in
          WindowClientCapabilities.create ~showDocument ()
        in
        ClientCapabilities.create ~window ()
      in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let run =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let uri = DocumentUri.of_path "foo.ml" in
      let open_ text =
        let textDocument =
          TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
        in
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = open_ "text 1" in
      let+ () = open_ "text 2" in
      ()
    in
    Fiber.fork_and_join_unit run_client (fun () ->
        run >>> Fiber.Ivar.read diagnostics >>> Client.stop client) );
  [%expect
    {|
    (* CR expect_test_collector: This test expectation appears to contain a backtrace.
       This is strongly discouraged as backtraces are fragile.
       Please change this test to not include a backtrace. *)

    Uncaught error when handling notification:
    {
      "params": {
        "textDocument": {
          "languageId": "ocaml",
          "text": "text 2",
          "uri": "file:///foo.ml",
          "version": 0
        }
      },
      "method": "textDocument/didOpen",
      "jsonrpc": "2.0"
    }
    Error:
    [ { exn =
          "File \"ocaml-lsp-server/src/ocaml_lsp_server.ml\", line 686, characters 4-10: Assertion failed"
      ; backtrace =
          "Raised at Ocaml_lsp_server.on_notification in file \"ocaml-lsp-server/src/ocaml_lsp_server.ml\", line 686, characters 4-72\n\
           Called from Fiber__Scheduler.exec in file \"fiber/src/scheduler.ml\", line 73, characters 8-11\n\
           "
      }
    ] |}]
