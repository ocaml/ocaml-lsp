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
      Text_document.apply_content_changes
        td
        [ TextDocumentContentChangeEvent.create ?range ~text:change () ]
    in
    Text_document.text td
  in
  let utf16 = test `UTF16 in
  let utf8 = test `UTF8 in
  let printf = Printf.printf in
  if String.equal utf16 utf8 then printf "result: %s\n" (String.escaped utf8)
  else (
    print_endline "[FAILURE] utf16 and utf8 disagree";
    printf "utf16: %s\n" (String.escaped utf16);
    printf "utf8:  %s\n" (String.escaped utf8))

let%expect_test "first line insert" =
  let range = tuple_range (0, 1) (0, 3) in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {|
    result: fXXXX bar baz |}]

let%expect_test "no range" =
  let range = None in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {|
    result: XXXX |}]

let%expect_test "replace second line" =
  let range = tuple_range (1, 0) (2, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXXX\n";
  [%expect {|
    result: foo\nXXXX\nbaz\n |}]

let%expect_test "edit in second line" =
  let range = tuple_range (1, 1) (1, 2) in
  test "foo\nbar\nbaz\n" range ~change:"-XXX-";
  [%expect {|
    result: foo\nb-XXX-r\nbaz\n |}]

let%expect_test "insert at the end" =
  let range = tuple_range (3, 1) (4, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXX";
  [%expect {|
   result: foo\n\bar\nbaz\nXXX |}]

let%expect_test "insert at the beginning" =
  let range = tuple_range (0, 0) (0, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXX\n";
  [%expect {|
    result: XXX\nfoo\n\bar\nbaz\n |}]

let%expect_test "replace first line" =
  let range = tuple_range (0, 0) (1, 0) in
  test "foo\nbar\n" range ~change:"baz\n";
  [%expect {|
    result: baz\nbar\n |}]

let%expect_test "beyond max char" =
  let range = tuple_range (0, 0) (0, 100) in
  test "foo\nbar\n" range ~change:"baz";
  [%expect {|
    result: baz\nbar\n |}]

let%expect_test "entire line without newline" =
  test "xxx\n" (tuple_range (0, 0) (0, 3)) ~change:"baz";
  [%expect {|
    result: baz\n |}];
  test "xxx\n" (tuple_range (0, 0) (0, 4)) ~change:"baz";
  [%expect {|
    result: baz\n |}];
  test "xxx\n" (tuple_range (0, 0) (1, 0)) ~change:"baz";
  [%expect {|
    result: baz |}]

let%expect_test "replace two lines" =
  test "a\nb\nc\n" (tuple_range (0, 0) (2, 0)) ~change:"XXX\n";
  [%expect {|
    result: XXX\nc\n |}]
