open! Test.Import

let project_root = Sys.getenv "DUNE_PROJECT_ROOT"
let fixture = Filename.concat project_root "ocaml-lsp-server/test/e2e-new/for_pp.ml"

let%expect_test "with-pp" =
  let dir = Test.temp_dir ~temp_dir:project_root "ocamllsp-with-pp-" in
  let path = Filename.concat dir "for_pp.ml" in
  let uri = DocumentUri.of_path path in
  Test.write_file path (Io.String_path.read_file fixture);
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.24)\n";
  Test.write_file
    (Filename.concat dir "dune")
    {|(library
 (name for_pp)
 (modules for_pp)
 (preprocess
  (action
   (run sed "s/world/universe/g" %{input-file}))))
|};
  Test.run_command ~cwd:dir "dune build";
  let position = Position.create ~line:0 ~character:9 in
  let handler =
    Client.Handler.make
      ~on_notification:(fun client _notification ->
        Client.state client;
        Fiber.return ())
      ()
  in
  let output =
    Test.run ~handler
    @@ fun client ->
    let run_client () = Test.start_client client in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        let text = Io.String_path.read_file path in
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () =
        let+ resp = Hover_helpers.hover ~uri client position in
        Hover_helpers.print_hover resp
      in
      let output = [%expect.output] in
      let* () = Client.request client Shutdown in
      let+ () = Client.stop client in
      output
    in
    Fiber.fork_and_join_unit run_client run
  in
  let (_ : string) = [%expect.output] in
  print_endline output;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "type universe" },
      "range": {
        "end": { "character": 13, "line": 0 },
        "start": { "character": 0, "line": 0 }
      }
    }|}]
;;
