open Test.Import

let%expect_test "code actions" =
  let source = {ocaml|
let foo = 123
|ocaml} in
  let handler =
    Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) ()
  in
  ( Test.run ~handler @@ fun client ->
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let run =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let uri = DocumentUri.of_path "foo.ml" in
      let* () =
        let textDocument =
          TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0
            ~text:source
        in
        Client.notification client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let+ resp =
        let range =
          let start = Position.create ~line:1 ~character:5 in
          let end_ = Position.create ~line:1 ~character:7 in
          Range.create ~start ~end_
        in
        let context = CodeActionContext.create ~diagnostics:[] () in
        let request =
          let textDocument = TextDocumentIdentifier.create ~uri in
          CodeActionParams.create ~textDocument ~range ~context ()
        in
        Client.request client (CodeAction request)
      in
      match resp with
      | None -> print_endline "no code actions"
      | Some code_actions ->
        print_endline "Code actions:";
        List.iter code_actions ~f:(fun ca ->
            let json =
              match ca with
              | `Command command -> Command.yojson_of_t command
              | `CodeAction ca -> CodeAction.yojson_of_t ca
            in
            Yojson.Safe.pretty_to_string ~std:false json |> print_endline)
    in
    Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client)
  );
  [%expect
    {|
    Code actions:
    {
      "title": "Type-annotate",
      "kind": "type-annotate",
      "isPreferred": false,
      "edit": {
        "documentChanges": [
          {
            "textDocument": { "uri": "file:///foo.ml", "version": 0 },
            "edits": [
              {
                "range": {
                  "start": { "line": 1, "character": 4 },
                  "end": { "line": 1, "character": 7 }
                },
                "newText": "(foo : int)"
              }
            ]
          }
        ]
      }
    }
    {
      "title": "Create foo.mli",
      "kind": "switch",
      "edit": {
        "documentChanges": [ { "kind": "create", "uri": "file:///foo.mli" } ]
      },
      "command": {
        "title": "Create foo.mli",
        "command": "ocamllsp/open-related-source",
        "arguments": [ "file:///foo.mli" ]
      }
    } |}]
