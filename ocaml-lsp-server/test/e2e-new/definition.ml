open Test.Import

let print_locations = function
  | None -> print_endline "[]"
  | Some locations -> Test.print_result (Locations.yojson_of_t locations)
;;

let definition client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (TextDocumentDefinition (DefinitionParams.create ~textDocument ~position ()))
;;

let%expect_test "returns location of a definition" =
  let source =
    {ocaml|let x = 43

let () =
  print_int x
|ocaml}
  in
  let req client =
    let* response = definition client (Position.create ~line:3 ~character:12) in
    print_locations response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 4, "line": 0 },
          "start": { "character": 4, "line": 0 }
        },
        "uri": "file:///test.ml"
      }
    ]
    |}]
;;
