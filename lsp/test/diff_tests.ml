open Stdune
module TextEdit = Lsp.Types.TextEdit
module Text_document = Lsp.Text_document
module TextDocumentContentChangeEvent = Lsp.Types.TextDocumentContentChangeEvent
module TextDocumentItem = Lsp.Types.TextDocumentItem
module DidOpenTextDocumentParams = Lsp.Types.DidOpenTextDocumentParams

let test ~from ~to_ =
  let edits = Lsp.Diff.edit ~from ~to_ in
  let json = `List (List.map ~f:TextEdit.yojson_of_t edits) in
  Yojson.Safe.pretty_to_string ~std:false json |> print_endline;
  let textDocument =
    let uri = Lsp.Uri.of_path "/tmp/test" in
    TextDocumentItem.create ~languageId:"test" ~text:from ~uri ~version:1
  in
  let text_document =
    Text_document.make
      ~position_encoding:`UTF8
      (DidOpenTextDocumentParams.create ~textDocument)
  in

  let changes =
    List.map edits ~f:(fun { TextEdit.range; newText = text } ->
        TextDocumentContentChangeEvent.create ~range ~text ())
  in
  let to_' =
    Text_document.apply_content_changes text_document changes
    |> Text_document.text
  in
  if not @@ String.equal to_ to_' then
    printfn "[FAILURE]\nresult: %S\nexpected: %S" to_' to_

let%expect_test "empty strings" =
  test ~from:"" ~to_:"";
  [%expect {|
    [] |}]

let%expect_test "from empty" =
  test ~from:"" ~to_:"foobar";
  [%expect
    {|
    [
      {
        "newText": "foobar",
        "range": {
          "end": { "character": 0, "line": 0 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ] |}];
  test ~from:"\n" ~to_:"foobar";
  [%expect
    {|
    [
      {
        "newText": "foobar",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ] |}]

let%expect_test "from empty - with newline" =
  test ~from:"" ~to_:"foobar\n";
  [%expect
    {|
  [
    {
      "newText": "foobar\n",
      "range": {
        "end": { "character": 0, "line": 0 },
        "start": { "character": 0, "line": 0 }
      }
    }
  ] |}]

let%expect_test "to empty" =
  test ~from:"foobar" ~to_:"";
  [%expect
    {|
    [
      {
        "newText": "",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ] |}]

let%expect_test "no change" =
  test ~from:"foobar" ~to_:"foobar";
  [%expect {|
    [] |}]

let%expect_test "multiline" =
  test ~from:"foo" ~to_:"baz\nbar\nxx\n";
  [%expect
    {|
    [
      {
        "newText": "baz\nbar\nxx\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ] |}]

let%expect_test "change a character" =
  test ~from:"xxx y xx" ~to_:"xxx z xx";
  [%expect
    {|
    [
      {
        "newText": "xxx z xx",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ] |}]

let%expect_test "delete empty line" =
  test ~from:"xxx\n\nyyy\n" ~to_:"xxx\nyyy\n";
  [%expect
    {|
    [
      {
        "newText": "",
        "range": {
          "end": { "character": 0, "line": 2 },
          "start": { "character": 0, "line": 1 }
        }
      }
    ] |}]
