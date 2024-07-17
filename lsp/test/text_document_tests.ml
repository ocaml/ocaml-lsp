open Lsp
open Lsp.Types
module List = ListLabels
module String = StringLabels

let printf = Printf.printf

let tuple_range start end_ =
  { Range.start =
      (let line, character = start in
       { Position.line; character })
  ; end_ =
      (let line, character = end_ in
       { Position.line; character })
  }
;;

let make_document ?(position_encoding = `UTF8) uri ~text =
  let td =
    let version = 1 in
    let languageId = "fake language" in
    let textDocument = { TextDocumentItem.uri; version; languageId; text } in
    Text_document.make ~position_encoding { DidOpenTextDocumentParams.textDocument }
  in
  Text_document.apply_content_changes
    td
    [ TextDocumentContentChangeEvent.create ~text () ]
;;

let test_general text changes =
  let test position_encoding =
    let td =
      let uri = DocumentUri.of_path "" in
      let version = 1 in
      let languageId = "fake language" in
      let textDocument = { TextDocumentItem.uri; version; languageId; text } in
      Text_document.make ~position_encoding { DidOpenTextDocumentParams.textDocument }
    in
    let td =
      Text_document.apply_content_changes
        td
        (ListLabels.map changes ~f:(fun (range, text) ->
           TextDocumentContentChangeEvent.create ?range ~text ()))
    in
    Text_document.text td
  in
  let utf8 = test `UTF8 in
  let utf16 = test `UTF16 in
  if String.equal utf16 utf8
  then printf "result: %s\n" (String.escaped utf8)
  else (
    print_endline "[FAILURE] utf16 and utf8 disagree";
    printf "utf16: %s\n" (String.escaped utf16);
    printf "utf8:  %s\n" (String.escaped utf8))
;;

let test text range ~change = test_general text [ Some range, change ]

let test_multiple text changes =
  test_general text (List.map changes ~f:(fun (range, text) -> Some range, text))
;;

let%expect_test "first line insert" =
  let range = tuple_range (0, 1) (0, 3) in
  test "foo bar baz" range ~change:"XXXX";
  [%expect {|
    result: fXXXX bar baz |}]
;;

let%expect_test "null edit" =
  test "foo bar" (tuple_range (0, 2) (0, 2)) ~change:"";
  [%expect {|
    result: foo bar |}]
;;

let%expect_test "no range" =
  test_general "foo bar baz" [ None, "XXXX" ];
  [%expect {|
    result: XXXX |}]
;;

let%expect_test "char by char" =
  test_multiple
    ""
    [ tuple_range (0, 0) (0, 0), "f"
    ; tuple_range (0, 1) (0, 1), "o"
    ; tuple_range (0, 2) (0, 2), "o"
    ];
  [%expect {|
    result: foo |}]
;;

let%expect_test "char by char - 2" =
  test_multiple
    "char by char - 2\n"
    [ tuple_range (1, 10) (1, 10), "b"
    ; tuple_range (1, 10) (1, 10), "a"
    ; tuple_range (1, 10) (1, 10), "r"
    ; tuple_range (1, 1) (1, 2), ""
    ];
  [%expect {|
    result: char by char - 2\nbr |}]
;;

let%expect_test "char by char - 3" =
  test_multiple
    "first line skip\nchar by char - 2\n"
    [ tuple_range (1, 4) (1, 5), ""
    ; tuple_range (1, 3) (1, 4), ""
    ; tuple_range (1, 3) (1, 3), "x"
    ];
  [%expect {|
    result: first line skip\nchaxby char - 2\n |}]
;;

let%expect_test "insert last" =
  test "x" (tuple_range (0, 1) (0, 1)) ~change:"y";
  [%expect {|
    result: xy |}];
  test "x\ny" (tuple_range (1, 1) (1, 1)) ~change:"z";
  [%expect {|
    result: x\nyz |}];
  test "x\ny" (tuple_range (1, 10) (1, 10)) ~change:"z";
  [%expect {|
    result: x\nyz |}]
;;

let%expect_test "replace second line" =
  let range = tuple_range (1, 0) (2, 0) in
  test "foo\nbar\nbaz\n" range ~change:"XXXX\n";
  [%expect {|
    result: foo\nXXXX\nbaz\n |}]
;;

let%expect_test "edit in second line" =
  let range = tuple_range (1, 1) (1, 2) in
  test "foo\nbar\nbaz\n" range ~change:"-XXX-";
  [%expect {|
    result: foo\nb-XXX-r\nbaz\n |}]
;;

let%expect_test "insert at the end" =
  let range = tuple_range (3, 0) (3, 0) in
  test "foo\nbar\nbaz\n" range ~change:"XXX";
  [%expect {|
    result: foo\nbar\nbaz\nXXX |}];
  let range = tuple_range (3, 0) (4, 0) in
  test "foo\nbar\nbaz\n" range ~change:"XXX";
  [%expect {|
    result: foo\nbar\nbaz\nXXX |}]
;;

let%expect_test "insert at the beginning" =
  let range = tuple_range (0, 0) (0, 0) in
  test "foo\n\bar\nbaz\n" range ~change:"XXX\n";
  [%expect {|
    result: XXX\nfoo\n\bar\nbaz\n |}]
;;

let%expect_test "insert in the middle" =
  test "ab" (tuple_range (0, 1) (0, 1)) ~change:"---";
  [%expect {|
    result: a---b |}]
;;

let%expect_test "replace first line" =
  let range = tuple_range (0, 0) (1, 0) in
  test "foo\nbar\n" range ~change:"baz\n";
  [%expect {|
    result: baz\nbar\n |}]
;;

let%expect_test "beyond max char" =
  let range = tuple_range (0, 0) (0, 100) in
  test "foo\nbar\n" range ~change:"baz";
  [%expect {|
    result: baz\nbar\n |}]
;;

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
;;

let%expect_test "replace two lines" =
  test "a\nb\nc\n" (tuple_range (0, 0) (2, 0)) ~change:"XXX\n";
  [%expect {|
    result: XXX\nc\n |}]
;;

let%expect_test "join lines" =
  test "a\nb" (tuple_range (0, 1) (1, 0)) ~change:"";
  [%expect {|
    result: ab |}]
;;

let%expect_test "remove text" =
  test "a---b" (tuple_range (0, 1) (0, 4)) ~change:"";
  [%expect {|
    result: ab |}]
;;

let%expect_test "remove newline - 1" =
  test "\n" (tuple_range (0, 0) (0, 1)) ~change:"";
  [%expect {|
    result: \n |}]
;;

let%expect_test "remove newlines - 2" =
  test_multiple "\nXXX\n" [ tuple_range (0, 0) (0, 1), "" ];
  [%expect {|
    result: \nXXX\n |}]
;;

let%expect_test "remove newlines - 3" =
  test_multiple
    "\nXXX\n\n"
    [ tuple_range (0, 0) (0, 1), ""; tuple_range (0, 1) (0, 2), "" ];
  [%expect {|
    result: \nXXX\n\n |}]
;;

let%expect_test "update when inserting a line at the end of the doc" =
  test "let x = 1;\n\nlet y = 2;" (tuple_range (2, 10) (2, 10)) ~change:"\n-ZZZ";
  [%expect {|
    result: let x = 1;\n\nlet y = 2;\n-ZZZ |}]
;;

let%expect_test "update when inserting a line at the end of the doc" =
  test_multiple
    "1\n2\n3\n"
    [ tuple_range (1, 9) (1, 9), "l"; tuple_range (1, 9) (1, 10), "" ];
  [%expect {|
    result: 1\n2l\n3\n |}]
;;

let%expect_test "absolute_position" =
  let text = "foo|bar\nbaz.x" in
  let td = make_document (Uri.of_path "foo.ml") ~text in
  let test (line, character) =
    let offset = Text_document.absolute_position td (Position.create ~line ~character) in
    printf "position: %d/%d\n" offset (String.length text)
  in
  test (0, 0);
  [%expect {| position: 0/13 |}];
  test (3, 0);
  [%expect {| position: 13/13 |}];
  test (1, 0);
  [%expect {| position: 8/13 |}];
  test (1, 100);
  [%expect {| position: 13/13 |}];
  test (0, 100);
  [%expect {| position: 7/13 |}];
  test (100, 0);
  [%expect {| position: 13/13 |}]
;;

let%expect_test "replace second line first line is \\n" =
  let range = tuple_range (1, 2) (1, 2) in
  let doc = make_document (Uri.of_path "foo.ml") ~text:"\nfoo\nbar\nbaz\n" in
  let edit = TextEdit.create ~newText:"change" ~range in
  let new_doc = Text_document.apply_text_document_edits doc [ edit ] in
  new_doc |> Text_document.text |> String.escaped |> print_endline;
  [%expect {|
    \nfochangeo\nbar\nbaz\n |}]
;;

let%expect_test "get position after change" =
  let range = tuple_range (1, 2) (1, 2) in
  let doc = make_document (Uri.of_path "foo.ml") ~text:"\nfoo\nbar\nbaz\n" in
  let edit = TextDocumentContentChangeEvent.create ~text:"change" ~range () in
  let new_doc = Text_document.apply_content_changes doc [ edit ] in
  let pos = Text_document.absolute_position new_doc range.start in
  new_doc |> Text_document.text |> String.escaped |> print_endline;
  printf "pos: %d\n" pos;
  [%expect {|
    \nfochangeo\nbar\nbaz\n
    pos: 22 |}]
;;
