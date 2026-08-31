open Test.Import

let print_locations = Test.print_option_list Location.yojson_of_t

let references client position ~includeDeclaration =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  let context = ReferenceContext.create ~includeDeclaration in
  Client.request
    client
    (TextDocumentReferences (ReferenceParams.create ~context ~textDocument ~position ()))
;;

let print_references label locations =
  print_endline label;
  print_locations locations
;;

let%expect_test "includeDeclaration is ignored" =
  let source =
    {ocaml|let num = 42
let sum = num + 13
let sum2 = sum + num
|ocaml}
  in
  let req client =
    let position = Position.create ~line:0 ~character:5 in
    (* Prime Merlin's project-occurrence lookup so both measured requests use the
       same synchronized state. *)
    let* _ = references client position ~includeDeclaration:true in
    let* all = references client position ~includeDeclaration:true in
    print_references "with declaration:" all;
    let* usages = references client position ~includeDeclaration:false in
    print_references "without declaration:" usages;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    with declaration:
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
    without declaration:
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
