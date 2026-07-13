open Test.Import

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc
;;

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let contents = really_input_string ic len in
  close_in ic;
  contents
;;

let setup_workspace () =
  let dir = Stdlib.Filename.temp_file "ocamllsp-declaration-" "" in
  Stdlib.Sys.remove dir;
  Unix.mkdir dir 0o700;
  write_file (Stdlib.Filename.concat dir "dune-project") "(lang dune 2.5)\n";
  write_file (Stdlib.Filename.concat dir "dune") "(library\n (name declaration_files))\n";
  write_file (Stdlib.Filename.concat dir "lib.ml") "let x = 1\n";
  write_file (Stdlib.Filename.concat dir "lib.mli") "val x : int\n";
  write_file (Stdlib.Filename.concat dir "main.ml") "let y = Lib.x\n";
  let command = Printf.sprintf "cd %s && dune build" (Stdlib.Filename.quote dir) in
  if Stdlib.Sys.command command <> 0 then failwith "dune build failed";
  dir
;;

let print_locations = function
  | None -> print_endline "[]"
  | Some (`Location locations) ->
    List.iter locations ~f:(fun (location : Location.t) ->
      print_endline (DocumentUri.to_path location.uri |> Stdlib.Filename.basename);
      Range.yojson_of_t location.range
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline)
  | Some (`LocationLink links) ->
    List.iter links ~f:(fun (location : LocationLink.t) ->
      print_endline (DocumentUri.to_path location.targetUri |> Stdlib.Filename.basename);
      Range.yojson_of_t location.targetRange
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline)
;;

let%expect_test "returns location of a declaration" =
  let dir = setup_workspace () in
  let path = Stdlib.Filename.concat dir "main.ml" in
  let uri = DocumentUri.of_path path in
  let source = read_file path in
  let stderr_path = if Sys.win32 then "NUL" else "/dev/null" in
  let stderr = Unix.openfile stderr_path [ O_WRONLY ] 0 in
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  (Test.run ~stderr ~handler
   @@ fun client ->
   let run_client () =
     Client.start
       client
       (InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ())
   in
   let run =
     let* (_ : InitializeResult.t) = Client.initialized client in
     let textDocument =
       TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
     in
     let* () =
       Client.notification
         client
         (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
     in
     let textDocument = TextDocumentIdentifier.create ~uri in
     let* response =
       Client.request
         client
         (TextDocumentDeclaration
            (TextDocumentPositionParams.create
               ~textDocument
               ~position:(Position.create ~line:0 ~character:13)))
     in
     print_locations response;
     let* () = Client.request client Shutdown in
     let* () = Fiber.Ivar.read diagnostics in
     Client.stop client
   in
   Fiber.fork_and_join_unit run_client (fun () -> run));
  Unix.close stderr;
  [%expect
    {|
    lib.mli
    {
      "end": { "character": 4, "line": 0 },
      "start": { "character": 4, "line": 0 }
    }
    |}]
;;
