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
  Test.run
  @@ fun client ->
  let run_client () =
    Client.start
      client
      (InitializeParams.create ~capabilities:(ClientCapabilities.create ()) ())
  in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* () = f client in
    Client.request client Shutdown
  in
  Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client)
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
