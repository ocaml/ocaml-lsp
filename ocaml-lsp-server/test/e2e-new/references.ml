open Test.Import

let print_locations = Test.print_option_list Location.yojson_of_t

let references client ~uri position ~includeDeclaration =
  let textDocument = TextDocumentIdentifier.create ~uri in
  let context = ReferenceContext.create ~includeDeclaration in
  Client.request
    client
    (TextDocumentReferences (ReferenceParams.create ~context ~textDocument ~position ()))
;;

let print_references label locations =
  print_endline label;
  print_locations locations
;;

let check_include_declaration client ~uri position ~print =
  (* Prime Merlin's project-occurrence lookup so both measured requests use the
     same synchronized state. *)
  let* (_ : Location.t list option) =
    references client ~uri position ~includeDeclaration:true
  in
  let* () =
    references client ~uri position ~includeDeclaration:true >>| print "with declaration:"
  in
  references client ~uri position ~includeDeclaration:false
  >>| print "without declaration:"
;;

let%expect_test "includeDeclaration is ignored" =
  let source =
    {ocaml|let num = 42
let sum = num + 13
let sum2 = sum + num
|ocaml}
  in
  let req client =
    check_include_declaration
      client
      ~uri:Helpers.uri
      (Position.create ~line:0 ~character:5)
      ~print:print_references
  in
  Helpers.test source req;
  [%expect
    {|
    with declaration:
    [
      {
        "range": {
          "end": { "character": 7, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///test.ml"
      },
      {
        "range": {
          "end": { "character": 13, "line": 1 },
          "start": { "character": 10, "line": 1 }
        },
        "uri": "file:///test.ml"
      },
      {
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 17, "line": 2 }
        },
        "uri": "file:///test.ml"
      }
    ]
    without declaration:
    [
      {
        "range": {
          "end": { "character": 7, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///test.ml"
      },
      {
        "range": {
          "end": { "character": 13, "line": 1 },
          "start": { "character": 10, "line": 1 }
        },
        "uri": "file:///test.ml"
      },
      {
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 17, "line": 2 }
        },
        "uri": "file:///test.ml"
      }
    ]
    |}]
;;

let setup_cross_file_workspace () =
  let dir = Test.temp_dir "ocamllsp-references-" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.0)\n";
  Test.write_file
    (Filename.concat dir "dune")
    "(library\n (name reference_files)\n (wrapped false))\n";
  Test.write_file (Filename.concat dir "lib.ml") "let value = 1\n";
  Test.write_file (Filename.concat dir "main.ml") "let result = Lib.value\n";
  Test.write_file (Filename.concat dir "other.ml") "let other = Lib.value\n";
  Test.run_command ~cwd:dir "dune build @ocaml-index";
  dir
;;

let print_project_references label locations =
  print_endline label;
  match locations with
  | None -> print_endline "null"
  | Some locations ->
    List.map locations ~f:(fun (location : Location.t) ->
      Filename.basename (DocumentUri.to_path location.uri), Range.to_string location.range)
    |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
    |> List.iter ~f:(fun (path, range) -> Printf.printf "%s: %s\n" path range)
;;

let%expect_test "cross-file includeDeclaration is ignored" =
  let dir = setup_cross_file_workspace () in
  let main_path = Filename.concat dir "main.ml" in
  let main_uri = DocumentUri.of_path main_path in
  let workspace =
    WorkspaceFolder.create ~uri:(DocumentUri.of_path dir) ~name:"references"
  in
  let stderr = Unix.openfile Test.null_device [ O_WRONLY ] 0 in
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  (Test.run_initialized ~cwd:dir ~stderr ~handler ~workspaceFolders:(Some [ workspace ])
   @@ fun client ->
   let textDocument =
     TextDocumentItem.create
       ~uri:main_uri
       ~languageId:(LanguageKind.Other "ocaml")
       ~version:0
       ~text:(Fs_io.read_file main_path |> Result.ok_exn)
   in
   let* () =
     Client.notification
       client
       (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
   in
   let* () =
     check_include_declaration
       client
       ~uri:main_uri
       (Position.create ~line:0 ~character:18)
       ~print:print_project_references
   in
   let* () = Client.request client Shutdown in
   Client.stop client);
  Unix.close stderr;
  [%expect
    {|
    with declaration:
    lib.ml: ((0, 4), (0, 9))
    main.ml: ((0, 17), (0, 22))
    other.ml: ((0, 16), (0, 21))
    without declaration:
    lib.ml: ((0, 4), (0, 9))
    main.ml: ((0, 17), (0, 22))
    other.ml: ((0, 16), (0, 21))
    |}]
;;
