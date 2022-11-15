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
  let test position_encoding =
    let td =
      let uri = DocumentUri.of_path "" in
      let version = 1 in
      let languageId = "fake language" in
      let textDocument = { TextDocumentItem.uri; version; languageId; text } in
      Text_document.make
        ~position_encoding
        { DidOpenTextDocumentParams.textDocument }
    in
    let td =
      Text_document.apply_content_change
        td
        (TextDocumentContentChangeEvent.create ?range ~text:change ())
    in
    (match position_encoding with
    | `UTF8 -> print_endline "UTF8:"
    | `UTF16 -> print_endline "UTF16:");
    print_endline (String.escaped (Text_document.text td))
  in
  test `UTF16;
  test `UTF8

let%expect_test "first line insert" =
  let range = tuple_range (0, 1) (0, 3) in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {|
    UTF16:
    fXXXX bar baz
    UTF8:
    fXXXX bar baz |}]

let%expect_test "no range" =
  let range = None in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {|
    UTF16:
    XXXX
    UTF8:
    XXXX |}]

let%expect_test "replace second line" =
  let range = tuple_range (1, 0) (2, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXXX\n";
  [%expect {|
    UTF16:
    foo\nXXXX\nbaz\n
    UTF8:
    foo\nXXXX\nbaz\n |}]

let%expect_test "edit in second line" =
  let range = tuple_range (1, 1) (1, 2) in
  test "foo\nbar\nbaz\n" range ~change:"-XXX-";
  [%expect
    {|
    UTF16:
    foo\nb-XXX-r\nbaz\n
    UTF8:
    foo\nb-XXX-r\nbaz\n |}]

let%expect_test "insert at the end" =
  let range = tuple_range (3, 1) (4, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXX";
  [%expect
    {|
   UTF16:
   foo\n\bar\nbaz\nXXX
   UTF8:
   foo\n\bar\nbaz\nXXX |}]

let%expect_test "insert at the beginning" =
  let range = tuple_range (0, 0) (0, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXX\n";
  [%expect
    {|
    UTF16:
    XXX\nfoo\n\bar\nbaz\n
    UTF8:
    XXX\nfoo\n\bar\nbaz\n |}]

let%expect_test "replace first line" =
  let range = tuple_range (0, 0) (1, 0) in
  test "foo\nbar\n" range ~change:"baz\n";
  [%expect {|
    UTF16:
    baz\nbar\n
    UTF8:
    baz\nbar\n |}]

let%expect_test "beyond max char" =
  let range = tuple_range (0, 0) (0, 100) in
  test "foo\nbar\n" range ~change:"baz\n";
  [%expect {|
    UTF16:
    baz\n
    UTF8:
    baz\nbar\n |}]
