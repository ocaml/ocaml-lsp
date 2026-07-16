open Test.Import

let uri = DocumentUri.of_string "file:///test-document.txt"
let text_document = TextDocumentIdentifier.create ~uri
let position line character = Position.create ~line ~character
let range start end_ = Range.create ~start ~end_

let print_document = function
  | None -> print_endline "<missing document>"
  | Some text -> print_endline text
;;

let open_document client source =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let change_document ?range ?rangeLength client ~version ~text =
  let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version in
  let contentChanges =
    [ TextDocumentContentChangeEvent.create ?range ?rangeLength ~text () ]
  in
  Client.notification
    client
    (TextDocumentDidChange
       (DidChangeTextDocumentParams.create ~textDocument ~contentChanges))
;;

let get_document client =
  let position = position 0 0 in
  Client.request
    client
    (DebugTextDocumentGet
       (TextDocumentPositionParams.create ~textDocument:text_document ~position))
;;

let run_document_test f =
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  Test.run ~handler
  @@ fun client ->
  let run_client () =
    Client.start
      client
      (InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ())
  in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = f client in
    let* () = Lev_fiber.Timer.sleepf 0.1 in
    let* () = Client.request client Shutdown in
    Client.notification client Exit
  in
  Fiber.fork_and_join_unit run_client (fun () -> run)
;;

let%expect_test "Manages unicode character ranges correctly" =
  run_document_test (fun client ->
    let source = "let x = 4\nlet y = \"a𐐀b\"" in
    let* () = open_document client source in
    let range = range (position 1 10) (position 1 12) in
    let* () = change_document client ~version:1 ~range ~text:"" in
    let+ document = get_document client in
    print_document document);
  [%expect
    {|
    let x = 4
    let y = "ab" |}]
;;

let%expect_test "updates in the middle of the line" =
  run_document_test (fun client ->
    let print_step document =
      print_document document;
      print_endline "---"
    in
    let source = "let x = 1;\n\nlet y = 2;" in
    let* () = open_document client source in
    let* document = get_document client in
    print_step document;
    let edit_range = range (position 2 5) (position 2 5) in
    let* () =
      change_document client ~version:1 ~range:edit_range ~rangeLength:0 ~text:"1"
    in
    let* document = get_document client in
    print_step document;
    let edit_range = range (position 2 5) (position 2 6) in
    let* () =
      change_document client ~version:2 ~range:edit_range ~rangeLength:1 ~text:""
    in
    let+ document = get_document client in
    print_document document);
  [%expect
    {|
    let x = 1;

    let y = 2;
    ---
    let x = 1;

    let y1 = 2;
    ---
    let x = 1;

    let y = 2; |}]
;;

let%expect_test "updates in at the start of the line" =
  run_document_test (fun client ->
    let source = "let x = 1;\n\nlet y = 2;" in
    let* () = open_document client source in
    let* document = get_document client in
    print_document document;
    print_endline "---";
    let edit_range = range (position 1 0) (position 1 0) in
    let* () =
      change_document client ~version:1 ~range:edit_range ~rangeLength:0 ~text:"s"
    in
    let+ document = get_document client in
    print_document document);
  [%expect
    {|
    let x = 1;

    let y = 2;
    ---
    let x = 1;
    s
    let y = 2; |}]
;;

let%expect_test "update when inserting a line" =
  run_document_test (fun client ->
    let source = "let x = 1;\n\nlet y = 2;" in
    let* () = open_document client source in
    let* document = get_document client in
    print_document document;
    print_endline "---";
    let edit_range = range (position 0 10) (position 0 10) in
    let* () =
      change_document
        client
        ~version:1
        ~range:edit_range
        ~rangeLength:0
        ~text:"\nlet x = 1;"
    in
    let+ document = get_document client in
    print_document document);
  [%expect
    {|
    let x = 1;

    let y = 2;
    ---
    let x = 1;
    let x = 1;

    let y = 2; |}]
;;

let%expect_test "update when inserting a line at the end of the doc" =
  run_document_test (fun client ->
    let source = "let x = 1;\n\nlet y = 2;" in
    let* () = open_document client source in
    let* document = get_document client in
    print_document document;
    print_endline "---";
    let edit_range = range (position 2 10) (position 2 10) in
    let* () =
      change_document
        client
        ~version:1
        ~range:edit_range
        ~rangeLength:0
        ~text:"\nlet y = 2;"
    in
    let+ document = get_document client in
    print_document document);
  [%expect
    {|
    let x = 1;

    let y = 2;
    ---
    let x = 1;

    let y = 2;
    let y = 2; |}]
;;

let%expect_test "update when deleting a line" =
  run_document_test (fun client ->
    let source = "let x = 1;\n\nlet y = 2;" in
    let* () = open_document client source in
    let* document = get_document client in
    print_document document;
    print_endline "---";
    let edit_range = range (position 0 0) (position 1 0) in
    let* () =
      change_document client ~version:1 ~range:edit_range ~rangeLength:11 ~text:""
    in
    let+ document = get_document client in
    print_document document);
  [%expect
    {|
    let x = 1;

    let y = 2;
    ---

    let y = 2; |}]
;;

let%expect_test "stores text document" =
  run_document_test (fun client ->
    let* () = open_document client "Hello, World!" in
    let+ document = get_document client in
    print_document document);
  [%expect {| Hello, World! |}]
;;
