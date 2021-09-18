open Lsp
open Lsp.Types

let tuple_range start end_ =
  Some
    { Range.start =
        (let line, character = start in
         { Position.line; character })
    ; end_ =
        (let line, character = end_ in
         { Position.line; character })
    }

let test text range ~change =
  let td =
    let uri = DocumentUri.of_path "" in
    let version = 1 in
    let languageId = "fake language" in
    let textDocument = { TextDocumentItem.uri; version; languageId; text } in
    Text_document.make { DidOpenTextDocumentParams.textDocument }
  in
  let td =
    Text_document.apply_content_change td
      (TextDocumentContentChangeEvent.create ?range ~text:change ())
  in
  print_endline (Text_document.text td)

let%expect_test "first line insert" =
  let range = tuple_range (0, 1) (0, 3) in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {| fXXXX bar baz |}]

let%expect_test "no range" =
  let range = None in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {| XXXX |}]

let%expect_test "replace second line" =
  let range = tuple_range (1, 0) (2, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXXX\n";
  [%expect {|
    foo 
    XXXX
    baz |}]

let%expect_test "edit in second line" =
  let range = tuple_range (1, 1) (1, 2) in
  test "foo\n\bar\nbaz\n" range ~change:"-XXX-";
  [%expect {|
    foo
    -XXX-r
    baz |}]

let%expect_test "insert at the end" =
  let range = tuple_range (3, 1) (4, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXX";
  [%expect {|
   foo
   ar
   baz
   XXX |}]

let%expect_test "insert at the beginning" =
  let range = tuple_range (0, 0) (0, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXX\n";
  [%expect {|
    XXX
    foo
    ar
    baz |}]
