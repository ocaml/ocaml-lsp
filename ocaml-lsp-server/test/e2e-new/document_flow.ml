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
      ~on_notification:(fun _ -> function
         | PublishDiagnostics _ -> Fiber.Mvar.write diagnostics ()
         | _ -> Fiber.return ())
      ~on_request:{ Client.Handler.on_request }
      ()
  in
  let capabilities =
    let window =
      let showDocument = ShowDocumentClientCapabilities.create ~support:true in
      WindowClientCapabilities.create ~showDocument ()
    in
    ClientCapabilities.create ~window ()
  in
  (Test.run_initialized ~handler ~capabilities
   @@ fun client ->
   let uri = DocumentUri.of_path "foo.ml" in
   let open_ text =
     let textDocument =
       TextDocumentItem.create
         ~uri
         ~languageId:(LanguageKind.Other "ocaml")
         ~version:0
         ~text
     in
     Client.notification
       client
       (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
   in
   let* () = open_ "text 1" in
   let* () = drain_diagnostics () in
   let* () = open_ "text 2" in
   let* () = drain_diagnostics () in
   Client.stop client);
  [%expect {| |}]
;;

let%expect_test "missing dune is reported as a diagnostic (#1417)" =
  let dir = Test.temp_dir "ocamllsp-missing-dune-" in
  let source = "let answer = 42\n" in
  let path = Stdlib.Filename.concat dir "main.ml" in
  Test.write_file (Stdlib.Filename.concat dir "dune-project") "(lang dune 3.24)\n";
  Test.write_file (Stdlib.Filename.concat dir "dune") "(executable (name main))\n";
  Test.write_file path source;
  let uri = DocumentUri.of_path path in
  let workspace = WorkspaceFolder.create ~uri:(DocumentUri.of_path dir) ~name:"test" in
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params ->
           let* filled = Fiber.Ivar.peek diagnostics in
           (match filled with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill diagnostics params)
         | _ -> Fiber.return ())
      ()
  in
  let stderr = Unix.openfile Test.null_device [ O_WRONLY ] 0 in
  (Test.run_initialized
     ~extra_env:[ "PATH=" ]
     ~workspaceFolders:(Some [ workspace ])
     ~handler
     ~stderr
   @@ fun client ->
   let textDocument =
     TextDocumentItem.create
       ~uri
       ~languageId:(LanguageKind.Other "ocaml")
       ~version:0
       ~text:source
   in
   let* () =
     Client.notification
       client
       (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
   in
   let* diagnostics = Fiber.Ivar.read diagnostics in
   List.iter diagnostics.diagnostics ~f:(fun (diagnostic : Diagnostic.t) ->
     match diagnostic.message with
     | `String message -> print_endline message
     | `MarkupContent { value; _ } -> print_endline value);
   let textDocument = TextDocumentIdentifier.create ~uri in
   let position = Position.create ~line:0 ~character:4 in
   let* result =
     Fiber.collect_errors (fun () ->
       Client.request
         client
         (TextDocumentHover (HoverParams.create ~textDocument ~position ())))
   in
   let* () =
     match result with
     | Error
         [ { Exn_with_backtrace.exn =
               Jsonrpc.Response.Error.E
                 { code = Jsonrpc.Response.Error.Code.InvalidRequest; message; data = _ }
           ; backtrace = _
           }
         ] ->
       String.replace_all message ~sub:(DocumentUri.to_string uri) ~by:"<document-uri>"
       |> print_endline;
       Fiber.return ()
     | Error errors -> Fiber.reraise_all errors
     | Ok _ ->
       print_endline "hover succeeded";
       Fiber.return ()
   in
   let* () = Client.request client Shutdown in
   Client.stop client);
  Unix.close stderr;
  [%expect
    {|
    dune binary not found
    hover succeeded
    |}]
;;
