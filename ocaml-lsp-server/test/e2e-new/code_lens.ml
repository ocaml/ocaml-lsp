open Test.Import

let change_config client params = Client.notification client (ChangeConfiguration params)

let codelens client textDocument =
  Client.request
    client
    (TextDocumentCodeLens
       { textDocument; workDoneToken = None; partialResultToken = None })
;;

let json_of_codelens cs = `List (List.map ~f:CodeLens.yojson_of_t cs)

let%expect_test "enable codelens for nested let bindings" =
  let source =
    {ocaml|
let toplevel = "Hello"

let func x = x

let f x =
  let y = 10 in
  let z = 3 in
  x + y + z
|ocaml}
  in
  let req client =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* () =
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "codelens", `Assoc [ "for_nested_bindings", `Bool true ] ]))
    in
    let* resp_codelens_toplevel = codelens client text_document in
    Test.print_result (json_of_codelens resp_codelens_toplevel);
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "command": { "command": "", "title": "int -> int" },
        "range": {
          "end": { "character": 11, "line": 8 },
          "start": { "character": 0, "line": 5 }
        }
      },
      {
        "command": { "command": "", "title": "int" },
        "range": {
          "end": { "character": 12, "line": 6 },
          "start": { "character": 2, "line": 6 }
        }
      },
      {
        "command": { "command": "", "title": "int" },
        "range": {
          "end": { "character": 11, "line": 7 },
          "start": { "character": 2, "line": 7 }
        }
      },
      {
        "command": { "command": "", "title": "'a -> 'a" },
        "range": {
          "end": { "character": 14, "line": 3 },
          "start": { "character": 0, "line": 3 }
        }
      },
      {
        "command": { "command": "", "title": "string" },
        "range": {
          "end": { "character": 22, "line": 1 },
          "start": { "character": 0, "line": 1 }
        }
      }
    ]
    |}]
;;

let%expect_test "enable codelens (default settings disable it for nested let binding)" =
  let source =
    {ocaml|
let x =
  let y = 10 in
  "Hello"

let () = ()
|ocaml}
  in
  let req client =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* () =
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "codelens", `Assoc [ "enable", `Bool true ] ]))
    in
    let* resp_codelens_toplevel = codelens client text_document in
    Test.print_result (json_of_codelens resp_codelens_toplevel);
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "command": { "command": "", "title": "string" },
        "range": {
          "end": { "character": 9, "line": 3 },
          "start": { "character": 0, "line": 1 }
        }
      }
    ]
    |}]
;;
