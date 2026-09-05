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
  let+ result =
    Client.request
      client
      (TextDocumentRename (RenameParams.create ~textDocument ~position ~newName ()))
  in
  Option.value_exn result
;;

let print_prepare_rename = function
  | None -> print_endline "null"
  | Some result -> PrepareRenameResult.yojson_of_t result |> Test.print_result
;;

let print_workspace_edit edit = WorkspaceEdit.yojson_of_t edit |> Test.print_result

let rec censor_backtraces = function
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (name, value) ->
         if String.equal name "backtrace"
         then name, `String "<censored>"
         else name, censor_backtraces value))
  | `List values -> `List (List.map values ~f:censor_backtraces)
  | json -> json
;;

let run ?(documentChanges = false) source f =
  Helpers.test ~capabilities:(capabilities ~documentChanges) source f
;;

let test_rename ~newName source_with_cursor =
  let source, position = Test.parse_cursor source_with_cursor in
  run source (fun client ->
    let+ response = rename ~newName client position in
    (match response.changes with
     | Some [ (_, edits) ] -> edits
     | None | Some _ -> failwith "expected edits for one document")
    |> Test.apply_edits source
    |> print_string)
;;

let rename_source =
  {ocaml|let num = 42
let num = num + 13
let num2 = num
|ocaml}
;;

let%expect_test "can reject invalid rename request" =
  run rename_source (fun client ->
    Position.create ~line:0 ~character:1 |> prepare_rename client >>| print_prepare_rename);
  [%expect {| null |}]
;;

let%expect_test "prepare rename leaks a lexer error on an astral character" =
  run "😀" (fun client ->
    let* result =
      Fiber.collect_errors (fun () ->
        prepare_rename client (Position.create ~line:0 ~character:0))
    in
    match result with
    | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
      ->
      Jsonrpc.Response.Error.yojson_of_t error |> censor_backtraces |> Test.print_result;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors
    | Ok response ->
      print_prepare_rename response;
      Fiber.return ());
  [%expect
    {|
    {
      "data": {
        "exn": "Ocaml_preprocess.Lexer_raw.Error(_, _)",
        "backtrace": "<censored>"
      },
      "code": -32603,
      "message": "uncaught exception"
    }
    |}]
;;

let%expect_test "prepare rename leaks Not_found on an incomplete local module" =
  run "let module X" (fun client ->
    Fiber.collect_errors (fun () ->
      prepare_rename client (Position.create ~line:0 ~character:12))
    >>= function
    | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
      ->
      Jsonrpc.Response.Error.yojson_of_t error |> censor_backtraces |> Test.print_result;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors
    | Ok response ->
      print_prepare_rename response;
      Fiber.return ());
  [%expect
    {|
    {
      "data": { "exn": "Not_found", "backtrace": "<censored>" },
      "code": -32603,
      "message": "uncaught exception"
    }
    |}]
;;

let%expect_test "rename deduplicates edits for an incomplete binding" =
  test_rename ~newName:"fuzz_renamed" "let rec ma$";
  [%expect {| let rec fuzz_renamed |}]
;;

let%expect_test "allows valid rename request" =
  run rename_source (fun client ->
    prepare_rename client (Position.create ~line:0 ~character:4) >>| print_prepare_rename);
  [%expect
    {|
    {
      "end": { "character": 7, "line": 0 },
      "start": { "character": 4, "line": 0 }
    }
    |}]
;;

let%expect_test "rename excludes operator parentheses (#1190)" =
  let source =
    {ocaml|let (^*$) a = a + 1
let b = (^*$) 1
|ocaml}
  in
  run source (fun client ->
    let position = Position.create ~line:0 ~character:6 in
    let* response = prepare_rename client position in
    print_prepare_rename response;
    (match response with
     | None -> ()
     | Some (`Range { Range.start; end_ }) ->
       let placeholder =
         String.sub source ~pos:start.character ~len:(end_.character - start.character)
       in
       Printf.printf "placeholder: %s\n" placeholder
     | Some (`PrepareRenamePlaceholder _ | `PrepareRenameDefaultBehavior _) ->
       assert false);
    let* response = rename ~newName:"^+$" client position in
    print_workspace_edit response;
    Fiber.return ());
  [%expect
    {|
    {
      "end": { "character": 8, "line": 0 },
      "start": { "character": 5, "line": 0 }
    }
    placeholder: ^*$
    {
      "changes": {
        "file:///test.ml": [
          {
            "newText": "^+$",
            "range": {
              "end": { "character": 12, "line": 1 },
              "start": { "character": 9, "line": 1 }
            }
          },
          {
            "newText": "^+$",
            "range": {
              "end": { "character": 8, "line": 0 },
              "start": { "character": 5, "line": 0 }
            }
          }
        ]
      }
    }
    |}]
;;

let%expect_test "rename record-punned variable also renames the field" =
  test_rename
    ~newName:"y"
    {ocaml|type t = { x : int }
let f $x = { x }
|ocaml};
  [%expect
    {|
    type t = { x : int }
    let f y = { y }
    |}]
;;

let%expect_test "rename record-punned pattern variable also renames the field" =
  test_rename
    ~newName:"y"
    {ocaml|type t = { x : int }
let get { $x } = x
|ocaml};
  [%expect
    {|
    type t = { x : int }
    let get { y } = y
    |}]
;;

let%expect_test "rename record field also renames a punned variable" =
  test_rename
    ~newName:"y"
    {ocaml|type t = { $x : int }
let f x = { x }
|ocaml};
  [%expect
    {|
    type t = { y : int }
    let f x = { y }
    |}]
;;

let%expect_test "rename value in a file without documentChanges capability" =
  run rename_source (fun client ->
    Position.create ~line:0 ~character:4 |> rename client >>| print_workspace_edit);
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
    Position.create ~line:0 ~character:4 |> rename client >>| print_workspace_edit);
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
  test_rename
    ~newName:"ident"
    {ocaml|let $foo x = x

let bar ~foo = foo ()

let () = bar ~foo
|ocaml};
  [%expect
    {|
    let ident x = x

    let bar ~foo = foo ()

    let () = bar ~foo:ident
    |}]
;;

let%expect_test "rename a var used as an optional argument" =
  test_rename
    ~newName:"sunit"
    {ocaml|let $foo = Some ()

let bar ?foo () = foo

;;
ignore (bar ?foo ())
|ocaml};
  [%expect
    {|
    let sunit = Some ()

    let bar ?foo () = foo

    ;;
    ignore (bar ?foo:sunit ())
    |}]
;;

let setup_multi_file_workspace
      ?(files =
        [ "lib.ml", "let value = 1\n"
        ; "main.ml", "let result = Lib.value\n"
        ; "other.ml", "let other = Lib.value\n"
        ])
      ()
  =
  let dir = Test.temp_dir "ocamllsp-rename-" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.0)\n";
  Test.write_file
    (Filename.concat dir "dune")
    "(library\n (name rename_files)\n (wrapped false))\n";
  List.iter files ~f:(fun (name, source) ->
    Test.write_file (Filename.concat dir name) source);
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

let test_project_rename ~newName ~request_file files =
  let request_source, { Range.start = position; end_ } =
    List.Assoc.find_exn files request_file ~equal:String.equal
    |> Code_actions.parse_selection
  in
  assert (Position.compare position end_ = 0);
  let files =
    List.map files ~f:(fun (name, source) ->
      if String.equal name request_file then name, request_source else name, source)
  in
  let dir = setup_multi_file_workspace ~files () in
  let uri name = Filename.concat dir name |> DocumentUri.of_path in
  let request_uri = uri request_file in
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
     open_project_document client ~uri:request_uri ~version:0 ~text:request_source
   in
   let textDocument = TextDocumentIdentifier.create ~uri:request_uri in
   let* response =
     Client.request
       client
       (TextDocumentRename (RenameParams.create ~textDocument ~position ~newName ()))
   in
   let document_changes =
     (Option.value_exn response).documentChanges |> Option.value_exn
   in
   List.iter document_changes ~f:(function
     | `TextDocumentEdit { textDocument = { uri; version = _ }; edits } ->
       let name = DocumentUri.to_path uri |> Filename.basename in
       let source = List.Assoc.find_exn files name ~equal:String.equal in
       let edits =
         List.map edits ~f:(function
           | `TextEdit edit -> edit
           | `AnnotatedTextEdit _ | `SnippetTextEdit _ ->
             failwith "unexpected annotated or snippet edit")
       in
       Printf.printf "%s:\n" name;
       Test.apply_edits source edits |> print_string
     | `CreateFile _ | `RenameFile _ | `DeleteFile _ ->
       failwith "unexpected resource operation");
   let* () = Client.request client Shutdown in
   Client.stop client);
  Unix.close stderr
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
   print_document_changes (Option.value_exn response);
   let* () = Client.request client Shutdown in
   Client.stop client);
  Unix.close stderr;
  [%expect
    {|
    lib.ml (version 7)
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
    other.ml (version null)
    {
      "newText": "renamed",
      "range": {
        "end": { "character": 21, "line": 0 },
        "start": { "character": 16, "line": 0 }
      }
    }
    |}]
;;

let%expect_test "rename cross-file record-punned variable also renames field" =
  test_project_rename
    ~newName:"renamed"
    ~request_file:"lib.ml"
    [ ( "lib.ml"
      , {ocaml|type t = { value : int }
let $value = 1
|ocaml}
      )
    ; ( "main.ml"
      , {ocaml|open Lib
let result : t = { value }
|ocaml}
      )
    ];
  [%expect
    {|
    lib.ml:
    type t = { value : int }
    let renamed = 1
    main.ml:
    open Lib
    let result : t = { renamed }
    |}]
;;

let%expect_test "rename cross-file punned record field also renames variable" =
  test_project_rename
    ~newName:"renamed"
    ~request_file:"lib.ml"
    [ ( "lib.ml"
      , {ocaml|type t = { $value : int }
let value = 1
|ocaml}
      )
    ; ( "main.ml"
      , {ocaml|open Lib
let result : t = { value }
|ocaml}
      )
    ];
  [%expect
    {|
    lib.ml:
    type t = { renamed : int }
    let value = 1
    main.ml:
    open Lib
    let result : t = { renamed }
    |}]
;;

let%expect_test "rename field without local declaration also renames punned variable" =
  test_project_rename
    ~newName:"renamed"
    ~request_file:"main.ml"
    [ "lib.ml", "type t = { value : int }\n"
    ; ( "main.ml"
      , {ocaml|open Lib
let value = 1
let explicit : t = { $value = 2 }
let punned : t = { value }
|ocaml}
      )
    ];
  [%expect
    {|
    lib.ml:
    type t = { renamed : int }
    main.ml:
    open Lib
    let value = 1
    let explicit : t = { renamed = 2 }
    let punned : t = { renamed }
    |}]
;;

let%expect_test "rename qualified record-punned variable also renames field" =
  test_rename
    ~newName:"y"
    {ocaml|module M = struct type t = { x : int } end
let f $x : M.t = { M.x }
|ocaml};
  [%expect
    {|
    module M = struct type t = { x : int } end
    let f y : M.t = { y }
    |}]
;;

let%expect_test "rename qualified punned field also renames variable" =
  test_rename
    ~newName:"y"
    {ocaml|module M = struct type t = { $x : int } end
let x = 1
let value : M.t = { M.x }
|ocaml};
  [%expect
    {|
    module M = struct type t = { y : int } end
    let x = 1
    let value : M.t = { M.y }
    |}]
;;

let%expect_test "equal ranges in different files are not record puns" =
  test_project_rename
    ~newName:"renamed"
    ~request_file:"lib.ml"
    [ ( "lib.ml"
      , {ocaml|type t = { value : int }
let $value = 1
let f value : t = { value }
|ocaml}
      )
    ; ( "main.ml"
      , {ocaml|let zero = 0
let one = 1
let result =    Lib.value
|ocaml}
      )
    ];
  [%expect
    {|
    lib.ml:
    type t = { value : int }
    let renamed = 1
    let f value : t = { value }
    main.ml:
    let zero = 0
    let one = 1
    let result =    Lib.renamed
    |}]
;;
