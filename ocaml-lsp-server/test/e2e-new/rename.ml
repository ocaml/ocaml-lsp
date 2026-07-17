open Test.Import

let capabilities ~documentChanges =
  let workspaceEdit = WorkspaceEditClientCapabilities.create ~documentChanges () in
  let workspace = WorkspaceClientCapabilities.create ~workspaceEdit () in
  ClientCapabilities.create ~workspace ()
;;

let prepare_rename client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (TextDocumentPrepareRename (PrepareRenameParams.create ~textDocument ~position ()))
;;

let rename ?(newName = "new_num") client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (TextDocumentRename (RenameParams.create ~textDocument ~position ~newName ()))
;;

let print_prepare_rename = function
  | None -> print_endline "null"
  | Some range -> Range.yojson_of_t range |> Test.print_result
;;

let print_workspace_edit edit = WorkspaceEdit.yojson_of_t edit |> Test.print_result

let run ?(documentChanges = false) source f =
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run ~handler (fun client ->
    let run_client () =
      Client.start
        client
        (InitializeParams.create ~capabilities:(capabilities ~documentChanges) ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create
          ~uri:Helpers.uri
          ~languageId:"ocaml"
          ~version:0
          ~text:source
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = f client in
      let* () = Client.request client Shutdown in
      let* () = Fiber.Ivar.read diagnostics in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run)
;;

let rename_source =
  {ocaml|let num = 42
let num = num + 13
let num2 = num
|ocaml}
;;

let%expect_test "can reject invalid rename request" =
  run rename_source (fun client ->
    let* response = prepare_rename client (Position.create ~line:0 ~character:1) in
    print_prepare_rename response;
    Fiber.return ());
  [%expect {| null |}]
;;

let%expect_test "allows valid rename request" =
  run rename_source (fun client ->
    let* response = prepare_rename client (Position.create ~line:0 ~character:4) in
    print_prepare_rename response;
    Fiber.return ());
  [%expect
    {|
    {
      "end": { "character": 7, "line": 0 },
      "start": { "character": 4, "line": 0 }
    }
    |}]
;;

let%expect_test "rename value in a file without documentChanges capability" =
  run rename_source (fun client ->
    let* response = rename client (Position.create ~line:0 ~character:4) in
    print_workspace_edit response;
    Fiber.return ());
  [%expect
    {|
    {
      "changes": {
        "file:///test.ml": [
          {
            "newText": "new_num",
            "range": {
              "end": { "character": 13, "line": 1 },
              "start": { "character": 10, "line": 1 }
            }
          },
          {
            "newText": "new_num",
            "range": {
              "end": { "character": 7, "line": 0 },
              "start": { "character": 4, "line": 0 }
            }
          }
        ]
      }
    }
    |}]
;;

let%expect_test "rename value in a file with documentChanges capability" =
  run ~documentChanges:true rename_source (fun client ->
    let* response = rename client (Position.create ~line:0 ~character:4) in
    print_workspace_edit response;
    Fiber.return ());
  [%expect
    {|
    {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "new_num",
              "range": {
                "end": { "character": 13, "line": 1 },
                "start": { "character": 10, "line": 1 }
              }
            },
            {
              "newText": "new_num",
              "range": {
                "end": { "character": 7, "line": 0 },
                "start": { "character": 4, "line": 0 }
              }
            }
          ],
          "textDocument": { "uri": "file:///test.ml", "version": 0 }
        }
      ]
    }
    |}]
;;

let%expect_test "rename a var used as a labelled argument" =
  let source =
    {ocaml|let foo x = x

let bar ~foo = foo ()

let () = bar ~foo
|ocaml}
  in
  run source (fun client ->
    let* response =
      rename ~newName:"ident" client (Position.create ~line:0 ~character:4)
    in
    print_workspace_edit response;
    Fiber.return ());
  [%expect
    {|
    {
      "changes": {
        "file:///test.ml": [
          {
            "newText": ":ident",
            "range": {
              "end": { "character": 17, "line": 4 },
              "start": { "character": 17, "line": 4 }
            }
          },
          {
            "newText": "ident",
            "range": {
              "end": { "character": 7, "line": 0 },
              "start": { "character": 4, "line": 0 }
            }
          }
        ]
      }
    }
    |}]
;;

let%expect_test "rename a var used as an optional argument" =
  let source =
    {ocaml|let foo = Some ()

let bar ?foo () = foo

;;
ignore (bar ?foo ())
|ocaml}
  in
  run source (fun client ->
    let* response =
      rename ~newName:"sunit" client (Position.create ~line:0 ~character:4)
    in
    print_workspace_edit response;
    Fiber.return ());
  [%expect
    {|
    {
      "changes": {
        "file:///test.ml": [
          {
            "newText": ":sunit",
            "range": {
              "end": { "character": 16, "line": 5 },
              "start": { "character": 16, "line": 5 }
            }
          },
          {
            "newText": "sunit",
            "range": {
              "end": { "character": 7, "line": 0 },
              "start": { "character": 4, "line": 0 }
            }
          }
        ]
      }
    }
    |}]
;;
