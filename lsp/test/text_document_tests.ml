open Lsp
open Lsp.Types
module List = ListLabels

let tuple_range start end_ =
  { Range.start =
      (let line, character = start in
       { Position.line; character })
  ; end_ =
      (let line, character = end_ in
       { Position.line; character })
  }

let test_general text changes =
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
        (ListLabels.map changes ~f:(fun (range, text) ->
             TextDocumentContentChangeEvent.create ?range ~text ()))
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

let test text range ~change = test_general text [ (Some range, change) ]

let test_multiple text changes =
  test_general
    text
    (List.map changes ~f:(fun (range, text) -> (Some range, text)))

let%expect_test "first line insert" =
  let range = tuple_range (0, 1) (0, 3) in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {|
    result: fXXXX bar baz |}]

let%expect_test "null edit" =
  test "foo bar" (tuple_range (0, 2) (0, 2)) ~change:"";
  [%expect {|
    result: foo bar |}]

let%expect_test "no range" =
  test_general "foo bar baz" [ (None, "XXXX") ];
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

let%expect_test "insert in the middle" =
  test "ab" (tuple_range (0, 1) (0, 1)) ~change:"---";
  [%expect {|
    result: a---b |}]

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

let%expect_test "join lines" =
  test "a\nb" (tuple_range (0, 1) (1, 0)) ~change:"";
  [%expect {|
    result: ab |}]

let%expect_test "remove text" =
  test "a---b" (tuple_range (0, 1) (0, 4)) ~change:"";
  [%expect {|
    result: ab |}]

let%expect_test "remove newline - 1" =
  test "\n" (tuple_range (0, 0) (0, 1)) ~change:"";
  [%expect {| result: \n |}]

let%expect_test "remove newlines - 2" =
  test_multiple "\nXXX\n" [ (tuple_range (0, 0) (0, 1), "") ];
  [%expect {|
    result: \nXXX\n |}]

let%expect_test "remove newlines - 3" =
  test_multiple
    "\nXXX\n\n"
    [ (tuple_range (0, 0) (0, 1), ""); (tuple_range (0, 1) (0, 2), "") ];
  [%expect
    {|
    [FAILURE] utf16 and utf8 disagree
    utf16: XX\n\n
    utf8:  \nXXX\n\n |}]
