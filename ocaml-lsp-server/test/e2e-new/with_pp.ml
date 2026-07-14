open! Test.Import

let path = Filename.concat (Sys.getcwd ()) "for_pp.ml"
let uri = DocumentUri.of_path path

let%expect_test "with-pp" =
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
