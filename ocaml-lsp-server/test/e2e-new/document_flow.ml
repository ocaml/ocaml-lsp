open Test.Import

let%expect_test "it should allow double opening the same document" =
  let diagnostics = Fiber.Mvar.create () in
  let drain_diagnostics () =
    let+ diagnostics = Fiber.Mvar.read diagnostics in
    PublishDiagnosticsParams.yojson_of_t diagnostics |> Test.print_result
  in
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
         | PublishDiagnostics params -> Fiber.Mvar.write diagnostics params
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
  [%expect
    {|
    {
      "diagnostics": [
        {
          "message": "Unbound value text",
          "range": {
            "end": { "character": 4, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///foo.ml"
    }
    {
      "diagnostics": [
        {
          "message": "Unbound value text",
          "range": {
            "end": { "character": 4, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///foo.ml"
    }
    |}]
;;

let%expect_test "missing dune diagnostic is cleared after dune is found (#1417)" =
  let dir = Test.temp_dir "ocamllsp-missing-dune-" in
  let bin_dir = Filename.concat dir "bin" in
  Unix.mkdir bin_dir 0o700;
  let dune = Bin.which "dune" |> Option.value_exn in
  let search_path =
    let path =
      Sys.getenv_opt "PATH"
      |> Option.value ~default:""
      |> String.split ~on:':'
      |> List.filter ~f:(fun dir -> not (Sys.file_exists (Filename.concat dir "dune")))
    in
    String.concat ~sep:":" (bin_dir :: path)
  in
  let source = "let answer = 42\n" in
  let path = Filename.concat dir "main.ml" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.24)\n";
  Test.write_file (Filename.concat dir "dune") "(executable (name main))\n";
  Test.write_file path source;
  Test.run_command ~cwd:dir (Filename.quote dune ^ " build");
  let uri = DocumentUri.of_path path in
  let workspace = WorkspaceFolder.create ~uri:(DocumentUri.of_path dir) ~name:"test" in
  let diagnostics = Fiber.Mvar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics params -> Fiber.Mvar.write diagnostics params
         | _ -> Fiber.return ())
      ()
  in
  let print_diagnostics (params : PublishDiagnosticsParams.t) =
    `List (List.map params.diagnostics ~f:Diagnostic.yojson_of_t) |> Test.print_result
  in
  let stderr = Unix.openfile Test.null_device [ O_WRONLY ] 0 in
  (Test.run_initialized
     ~extra_env:[ "PATH=" ^ search_path ]
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
   let* initial_diagnostics = Fiber.Mvar.read diagnostics in
   print_endline "before dune is found:";
   print_diagnostics initial_diagnostics;
   Unix.symlink dune (Filename.concat bin_dir "dune");
   let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version:1 in
   let contentChanges =
     [ `TextDocumentContentChangeWholeDocument
         (TextDocumentContentChangeWholeDocument.create ~text:source)
     ]
   in
   let* () =
     Client.notification
       client
       (TextDocumentDidChange
          (DidChangeTextDocumentParams.create ~textDocument ~contentChanges))
   in
   let* updated_diagnostics = Fiber.Mvar.read diagnostics in
   print_endline "after dune is found:";
   print_diagnostics updated_diagnostics;
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
       String.substr_replace_all
         message
         ~pattern:(DocumentUri.to_string uri)
         ~with_:"<document-uri>"
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
    before dune is found:
    [
      {
        "message": "dune binary not found",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        },
        "severity": 1,
        "source": "ocamllsp"
      }
    ]
    after dune is found:
    []
    hover succeeded
    |}]
;;
