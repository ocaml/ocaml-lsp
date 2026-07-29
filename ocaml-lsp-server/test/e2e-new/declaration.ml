open Test.Import

let setup_workspace () =
  let dir = Test.temp_dir "ocamllsp-declaration-" in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 2.5)\n";
  Test.write_file (Filename.concat dir "dune") "(library\n (name declaration_files))\n";
  Test.write_file (Filename.concat dir "lib.ml") "let x = 1\n";
  Test.write_file (Filename.concat dir "lib.mli") "val x : int\n";
  Test.write_file (Filename.concat dir "main.ml") "let y = Lib.x\n";
  Test.run_command ~cwd:dir "dune build";
  dir
;;

let print_location (location : Location.t) =
  print_endline (DocumentUri.to_path location.uri |> Filename.basename);
  Range.yojson_of_t location.range
  |> Yojson.Safe.pretty_to_string ~std:false
  |> print_endline
;;

let print_locations = function
  | None -> print_endline "[]"
  | Some (`Definition (`Location location)) -> print_location location
  | Some (`Definition (`List locations)) -> List.iter locations ~f:print_location
  | Some (`DefinitionLink links) ->
    List.iter links ~f:(fun (location : LocationLink.t) ->
      print_endline (DocumentUri.to_path location.targetUri |> Filename.basename);
      Range.yojson_of_t location.targetRange
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline)
  | Some (`Declaration (`Location location)) -> print_location location
  | Some (`Declaration (`List locations)) -> List.iter locations ~f:print_location
  | Some (`DeclarationLink links) ->
    List.iter links ~f:(fun (location : LocationLink.t) ->
      print_endline (DocumentUri.to_path location.targetUri |> Filename.basename);
      Range.yojson_of_t location.targetRange
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline)
;;

let%expect_test "distinguishes a definition from a declaration" =
  let dir = setup_workspace () in
  let path = Filename.concat dir "main.ml" in
  let uri = DocumentUri.of_path path in
  let source = Fs_io.read_file path |> Result.ok_exn in
  let stderr = Unix.openfile Test.null_device [ O_WRONLY ] 0 in
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  (Test.run_initialized ~stderr ~handler
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
   let textDocument = TextDocumentIdentifier.create ~uri in
   let position = Position.create ~line:0 ~character:13 in
   let* definition =
     Client.request
       client
       (TextDocumentDefinition (DefinitionParams.create ~textDocument ~position ()))
   in
   print_endline "definition:";
   print_locations definition;
   let* declaration =
     Client.request
       client
       (TextDocumentDeclaration (DeclarationParams.create ~textDocument ~position ()))
   in
   print_endline "declaration:";
   print_locations declaration;
   let* () = Client.request client Shutdown in
   let* () = Fiber.Ivar.read diagnostics in
   Client.stop client);
  Unix.close stderr;
  [%expect
    {|
    definition:
    lib.ml
    {
      "end": { "character": 4, "line": 0 },
      "start": { "character": 4, "line": 0 }
    }
    declaration:
    lib.mli
    {
      "end": { "character": 4, "line": 0 },
      "start": { "character": 4, "line": 0 }
    }
    |}]
;;
