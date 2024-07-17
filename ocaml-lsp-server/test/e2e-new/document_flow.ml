open Test.Import

let%expect_test "it should allow double opening the same document" =
  let diagnostics = Fiber.Mvar.create () in
  let drain_diagnostics () = Fiber.Mvar.read diagnostics in
  let handler =
    let on_request
      (type resp state)
      (client : state Client.t)
      (req : resp Lsp.Server_request.t)
      : (resp Lsp_fiber.Rpc.Reply.t * state) Fiber.t
      =
      match req with
      | Lsp.Server_request.ClientUnregisterCapability _ ->
        let state = Client.state client in
        Fiber.return (Lsp_fiber.Rpc.Reply.now (), state)
      | _ -> assert false
    in
    Client.Handler.make
      ~on_notification:(fun _ ->
        function
        | PublishDiagnostics _ -> Fiber.Mvar.write diagnostics ()
        | _ -> Fiber.return ())
      ~on_request:{ Client.Handler.on_request }
      ()
  in
  (Test.run ~handler
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
     let* () = drain_diagnostics () in
     let+ () = open_ "text 2" in
     ()
   in
   Fiber.fork_and_join_unit run_client (fun () ->
     run >>> drain_diagnostics () >>> Client.stop client));
  [%expect {| |}]
;;
