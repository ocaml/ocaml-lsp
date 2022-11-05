open Test.Import

let iter_code_actions ?(path = "foo.ml") ~source range k =
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:
        (fun _ -> function
          | PublishDiagnostics _ -> Fiber.Ivar.fill diagnostics ()
          | _ -> Fiber.return ())
      ()
  in
  Test.run ~handler @@ fun client ->
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
    let uri = DocumentUri.of_path path in
    let* () =
      let textDocument =
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
      in
      Client.notification
        client
        (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
    in
    let+ resp =
      let context = CodeActionContext.create ~diagnostics:[] () in
      let request =
        let textDocument = TextDocumentIdentifier.create ~uri in
        CodeActionParams.create ~textDocument ~range ~context ()
      in
      Client.request client (CodeAction request)
    in
    k resp
  in
  Fiber.fork_and_join_unit run_client (fun () ->
      run >>> Fiber.Ivar.read diagnostics >>> Client.stop client)

let print_code_actions ?(path = "foo.ml") source range =
  iter_code_actions ~path ~source range (function
      | None -> print_endline "no code actions"
      | Some code_actions ->
        print_endline "Code actions:";
        List.iter code_actions ~f:(fun ca ->
            let json =
              match ca with
              | `Command command -> Command.yojson_of_t command
              | `CodeAction ca -> CodeAction.yojson_of_t ca
            in
            Yojson.Safe.pretty_to_string ~std:false json |> print_endline))

let%expect_test "code actions" =
  let source = {ocaml|
let foo = 123
|ocaml} in
  let range =
    let start = Position.create ~line:1 ~character:5 in
    let end_ = Position.create ~line:1 ~character:7 in
    Range.create ~start ~end_
  in
  print_code_actions source range;
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "(foo : int)",
                "range": {
                  "end": { "character": 7, "line": 1 },
                  "start": { "character": 4, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "type-annotate",
      "title": "Type-annotate"
    }
    {
      "command": {
        "arguments": [ "file:///foo.mli" ],
        "command": "ocamllsp/open-related-source",
        "title": "Create foo.mli"
      },
      "edit": {
        "documentChanges": [ { "kind": "create", "uri": "file:///foo.mli" } ]
      },
      "kind": "switch",
      "title": "Create foo.mli"
    } |}]
