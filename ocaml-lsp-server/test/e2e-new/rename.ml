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
  Helpers.test ~capabilities:(capabilities ~documentChanges) source f
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

let setup_multi_file_workspace () =
  let dir = Test.temp_dir "ocamllsp-rename-" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.0)\n";
  Test.write_file
    (Filename.concat dir "dune")
    "(library\n (name rename_files)\n (wrapped false))\n";
  Test.write_file (Filename.concat dir "lib.ml") "let value = 1\n";
  Test.write_file (Filename.concat dir "main.ml") "let result = Lib.value\n";
  Test.write_file (Filename.concat dir "other.ml") "let other = Lib.value\n";
  Test.run_command ~cwd:dir "dune build @ocaml-index";
  dir
;;

let open_project_document client ~uri ~version ~text =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:(LanguageKind.Other "ocaml") ~version ~text
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let print_document_changes (edit : WorkspaceEdit.t) =
  match edit.documentChanges with
  | None -> print_endline "missing documentChanges"
  | Some changes ->
    List.iter changes ~f:(function
      | `TextDocumentEdit { textDocument = { uri; version }; edits } ->
        let version = Option.value_map version ~default:"null" ~f:Int.to_string in
        Printf.printf
          "%s (version %s)\n"
          (DocumentUri.to_path uri |> Filename.basename)
          version;
        List.iter edits ~f:(function
          | `TextEdit edit -> TextEdit.yojson_of_t edit |> Test.print_result
          | `AnnotatedTextEdit _ | `SnippetTextEdit _ ->
            failwith "unexpected annotated or snippet edit")
      | `CreateFile _ | `RenameFile _ | `DeleteFile _ ->
        failwith "unexpected resource operation")
;;

let%expect_test "rename a symbol across open and closed files" =
  let dir = setup_multi_file_workspace () in
  let uri name = Filename.concat dir name |> DocumentUri.of_path in
  let lib_uri = uri "lib.ml" in
  let main_uri = uri "main.ml" in
  let workspace = WorkspaceFolder.create ~uri:(DocumentUri.of_path dir) ~name:"rename" in
  let stderr = Unix.openfile Test.null_device [ O_WRONLY ] 0 in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  (Test.run_initialized
     ~cwd:dir
     ~stderr
     ~handler
     ~capabilities:(capabilities ~documentChanges:true)
     ~workspaceFolders:(Some [ workspace ])
   @@ fun client ->
   let* () =
     open_project_document client ~uri:lib_uri ~version:7 ~text:"let value = 1\n"
   in
   let* () =
     open_project_document
       client
       ~uri:main_uri
       ~version:3
       ~text:"let result = Lib.value\n"
   in
   let textDocument = TextDocumentIdentifier.create ~uri:main_uri in
   let* response =
     Client.request
       client
       (TextDocumentRename
          (RenameParams.create
             ~textDocument
             ~position:(Position.create ~line:0 ~character:20)
             ~newName:"renamed"
             ()))
   in
   print_document_changes response;
   let* () = Client.request client Shutdown in
   Client.stop client);
  Unix.close stderr;
  [%expect
    {|
    lib.ml (version 3)
    {
      "newText": "renamed",
      "range": {
        "end": { "character": 9, "line": 0 },
        "start": { "character": 4, "line": 0 }
      }
    }
    main.ml (version 3)
    {
      "newText": "renamed",
      "range": {
        "end": { "character": 22, "line": 0 },
        "start": { "character": 17, "line": 0 }
      }
    }
    other.ml (version 3)
    {
      "newText": "renamed",
      "range": {
        "end": { "character": 21, "line": 0 },
        "start": { "character": 16, "line": 0 }
      }
    }
    |}]
;;
