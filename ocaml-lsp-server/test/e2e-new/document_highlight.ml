open Test.Import

let print_highlights = Test.print_option_list DocumentHighlight.yojson_of_t

let document_highlight client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (TextDocumentHighlight (DocumentHighlightParams.create ~textDocument ~position ()))
;;

let%expect_test "highlight references in a file" =
  let source =
    {ocaml|let num = 42
let sum = num + 13
let sum2 = sum + num
|ocaml}
  in
  let req client =
    let* response = document_highlight client (Position.create ~line:0 ~character:4) in
    print_highlights response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "kind": 1,
        "range": {
          "end": { "character": 7, "line": 0 },
          "start": { "character": 4, "line": 0 }
        }
      },
      {
        "kind": 1,
        "range": {
          "end": { "character": 13, "line": 1 },
          "start": { "character": 10, "line": 1 }
        }
      },
      {
        "kind": 1,
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 17, "line": 2 }
        }
      }
    ]
    |}]
;;

let%expect_test "highlight module names" =
  let source =
    {ocaml|module M = struct
  let x = 1
end

let y = M.x
module N = M
|ocaml}
  in
  let req client =
    let* response = document_highlight client (Position.create ~line:0 ~character:7) in
    print_highlights response;
    let* response = document_highlight client (Position.create ~line:4 ~character:8) in
    print_highlights response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "kind": 1,
        "range": {
          "end": { "character": 8, "line": 0 },
          "start": { "character": 7, "line": 0 }
        }
      },
      {
        "kind": 1,
        "range": {
          "end": { "character": 9, "line": 4 },
          "start": { "character": 8, "line": 4 }
        }
      },
      {
        "kind": 1,
        "range": {
          "end": { "character": 12, "line": 5 },
          "start": { "character": 11, "line": 5 }
        }
      }
    ]
    [
      {
        "kind": 1,
        "range": {
          "end": { "character": 8, "line": 0 },
          "start": { "character": 7, "line": 0 }
        }
      },
      {
        "kind": 1,
        "range": {
          "end": { "character": 9, "line": 4 },
          "start": { "character": 8, "line": 4 }
        }
      },
      {
        "kind": 1,
        "range": {
          "end": { "character": 12, "line": 5 },
          "start": { "character": 11, "line": 5 }
        }
      }
    ]
    |}]
;;
