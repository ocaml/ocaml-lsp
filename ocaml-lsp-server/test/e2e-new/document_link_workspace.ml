open Test.Import

let print_link (link : DocumentLink.t) =
  let target =
    Option.map link.target ~f:(fun target ->
      match DocumentUri.to_string target |> String.chop_prefix ~prefix:"file://" with
      | None -> DocumentUri.to_string target
      | Some path -> Filename.basename path)
  in
  DocumentLink.yojson_of_t
    (DocumentLink.create
       ~range:link.range
       ?target:(Option.map target ~f:DocumentUri.of_string)
       ())
;;

let print_links = Test.print_option_list ~none:"null" print_link

let%expect_test "resolves a cross-reference into another module" =
  let dir = Test.temp_dir "ocamllsp-document-link-" in
  let source =
    {|(** Wraps {!Helper.describe} for a {!Helper.color}, in particular {!Helper.Red}. *)
let show c = Helper.describe c
|}
  in
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 2.5)\n";
  Test.write_file (Filename.concat dir "dune") "(library\n (name document_link_files))\n";
  Test.write_file
    (Filename.concat dir "helper.ml")
    "type color =\n  | Red\n\nlet describe Red = \"red\"\n";
  Test.write_file (Filename.concat dir "user.ml") source;
  Test.run_command ~cwd:dir "dune build";
  let uri = DocumentUri.of_path (Filename.concat dir "user.ml") in
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
   let* response =
     Client.request client (TextDocumentLink (DocumentLinkParams.create ~textDocument ()))
   in
   let* resolved =
     Fiber.sequential_map (Option.value response ~default:[]) ~f:(fun link ->
       Client.request client (TextDocumentLinkResolve link))
   in
   print_links (Some resolved);
   let* () = Client.request client Shutdown in
   let* () = Fiber.Ivar.read diagnostics in
   Client.stop client);
  Unix.close stderr;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 28, "line": 0 },
          "start": { "character": 10, "line": 0 }
        },
        "target": "file:///helper.ml#L4,5"
      },
      {
        "range": {
          "end": { "character": 50, "line": 0 },
          "start": { "character": 35, "line": 0 }
        },
        "target": "file:///helper.ml#L1,6"
      },
      {
        "range": {
          "end": { "character": 79, "line": 0 },
          "start": { "character": 66, "line": 0 }
        },
        "target": "file:///helper.ml#L2,5"
      }
    ]
    |}]
;;
