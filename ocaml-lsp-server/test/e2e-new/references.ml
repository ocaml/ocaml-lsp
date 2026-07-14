open Test.Import

let print_locations = function
  | None -> print_endline "[]"
  | Some locations ->
    Test.print_result (`List (List.map locations ~f:Location.yojson_of_t))
;;

let references client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  let context = ReferenceContext.create ~includeDeclaration:false in
  Client.request
    client
    (TextDocumentReferences (ReferenceParams.create ~context ~textDocument ~position ()))
;;

let%expect_test "finds references in a file" =
  let source =
    {ocaml|let num = 42
let sum = num + 13
let sum2 = sum + num
|ocaml}
  in
  let req client =
    let* response = references client (Position.create ~line:0 ~character:4) in
    print_locations response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 7, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///test.ml"
      },
      {
        "range": {
          "end": { "character": 13, "line": 1 },
          "start": { "character": 10, "line": 1 }
        },
        "uri": "file:///test.ml"
      },
      {
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 17, "line": 2 }
        },
        "uri": "file:///test.ml"
      }
    ]
    |}]
;;
