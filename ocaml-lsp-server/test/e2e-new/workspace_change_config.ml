open Test.Import

let codelens client textDocument =
  Client.request
    client
    (TextDocumentCodeLens
       { textDocument; workDoneToken = None; partialResultToken = None })
;;

let%expect_test "can add the first workspace folder after initialization" =
  let stderr_path, stderr_chan =
    Stdlib.Filename.open_temp_file "ocamllsp-workspace" ".log"
  in
  let stderr = Unix.descr_of_out_channel stderr_chan in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  Test.run ~handler ~stderr (fun client ->
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start
        client
        (InitializeParams.create ~capabilities ~workspaceFolders:None ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let folder =
        WorkspaceFolder.create ~uri:(DocumentUri.of_path "/tmp/new-workspace") ~name:"new"
      in
      let event = WorkspaceFoldersChangeEvent.create ~added:[ folder ] ~removed:[] in
      let change = DidChangeWorkspaceFoldersParams.create ~event in
      let* () = Client.notification client (ChangeWorkspaceFolders change) in
      let* () = Client.request client Shutdown in
      Client.notification client Exit
    in
    Fiber.fork_and_join_unit run_client run);
  Stdlib.close_out stderr_chan;
  let stderr = Io.String_path.read_file stderr_path in
  Stdlib.Sys.remove stderr_path;
  let failed = Base.String.is_substring stderr ~substring:"Assertion failed" in
  Printf.printf "workspace update error: %b\n" failed;
  [%expect {| workspace update error: false |}]
;;

let%expect_test "disable codelens" =
  let source =
    {ocaml|
let string = "Hello"
|ocaml}
  in
  let req client =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* () =
      Lsp_helpers.change_config
        ~client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "codelens", `Assoc [ "enable", `Bool false ] ]))
    in
    let* resp_codelens_disabled = codelens client text_document in
    print_endline ("CodeLens found: " ^ string_of_int (List.length resp_codelens_disabled));
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| CodeLens found: 0 |}]
;;

let%expect_test "enable hover extended" =
  let source = Hover_helpers.hover_reference_source in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    let* () =
      Lsp_helpers.change_config
        ~client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "extendedHover", `Assoc [ "enable", `Bool true ] ]))
    in
    (* The first hover request has verbosity = 0 *)
    let* _ = Hover_helpers.hover client position in
    (* The second hover request has verbosity = 1 *)
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
;;
