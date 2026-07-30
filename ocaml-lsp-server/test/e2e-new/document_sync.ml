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
    TextDocumentItem.create
      ~uri
      ~languageId:(LanguageKind.Other "ocaml")
      ~version:0
      ~text:source
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let close_document client =
  Client.notification
    client
    (TextDocumentDidClose
       (DidCloseTextDocumentParams.create
          ~textDocument:(TextDocumentIdentifier.create ~uri)))
;;

let save_document client =
  Client.notification
    client
    (DidSaveTextDocument
       (DidSaveTextDocumentParams.create
          ~textDocument:(TextDocumentIdentifier.create ~uri)
          ()))
;;

let change_document ?range ?rangeLength client ~version ~text =
  let textDocument = VersionedTextDocumentIdentifier.create ~uri ~version in
  let contentChanges =
    let change : TextDocumentContentChangeEvent.t =
      match range with
      | None ->
        `TextDocumentContentChangeWholeDocument
          (TextDocumentContentChangeWholeDocument.create ~text)
      | Some range ->
        `TextDocumentContentChangePartial
          (TextDocumentContentChangePartial.create ?rangeLength ~range ~text ())
    in
    [ change ]
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
  Test.run_initialized ~handler
  @@ fun client ->
  let* () = f client in
  Test.exit_client client
;;

let%expect_test "close, save after close, and reopen a document" =
  run_document_test (fun client ->
    let* () = open_document client "let first = 1" in
    let* first = get_document client in
    print_endline "open:";
    print_document first;
    let* () = close_document client in
    let* () = save_document client in
    let* closed = Fiber.collect_errors (fun () -> get_document client) in
    let* () =
      match closed with
      | Error
          [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
        ->
        Printf.printf
          "after close: code=%s message=%s\n"
          (Jsonrpc.Response.Error.Code.to_string error.code)
          (String.substr_replace_all
             error.message
             ~pattern:(DocumentUri.to_string uri)
             ~with_:"<document-uri>");
        Fiber.return ()
      | Error errors -> Fiber.reraise_all errors
      | Ok document ->
        print_endline "after close unexpectedly succeeded:";
        print_document document;
        Fiber.return ()
    in
    let* () = open_document client "let second = 2" in
    let+ reopened = get_document client in
    print_endline "reopened:";
    print_document reopened);
  [%expect
    {|
    open:
    let first = 1
    after close unexpectedly succeeded:
    <missing document>
    reopened:
    let second = 2
    |}]
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

let%expect_test "updates text document" =
  run_document_test (fun client ->
    let* () = open_document client "Hello, World!" in
    let* () = change_document client ~version:1 ~text:"Hello again!" in
    let+ document = get_document client in
    print_document document);
  [%expect {| Hello again! |}]
;;
