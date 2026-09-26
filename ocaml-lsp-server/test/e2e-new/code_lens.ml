open Test.Import

let codelens client textDocument =
  let+ result =
    Client.request
      client
      (TextDocumentCodeLens
         { textDocument; workDoneToken = None; partialResultToken = None })
  in
  Option.value_exn result
;;

let json_of_codelens cs = `List (List.map ~f:CodeLens.yojson_of_t cs)

let%expect_test "returns codeLens for a module" =
  let source =
    {ocaml|let num = 42
let string = "Hello"

module M = struct
  let m a b = a + b
end
|ocaml}
  in
  let req client =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* () =
      Lsp_helpers.change_config
        ~client
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
          "end": { "character": 20, "line": 1 },
          "start": { "character": 0, "line": 1 }
        }
      },
      {
        "command": { "command": "", "title": "int" },
        "range": {
          "end": { "character": 12, "line": 0 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]
;;

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
      Lsp_helpers.change_config
        ~client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "codelens", `Assoc [ "forNestedBindings", `Bool true ] ]))
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
      Lsp_helpers.change_config
        ~client
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

let%expect_test "code lenses do not show unrelated type aliases" =
  let source =
    {ocaml|type nat = int

let rec fact (n : nat) : nat = if n = 0 then 1 else n * fact (n - 1)
let incr (x : int) : int = x + 1
let incr2 x = x + 1
|ocaml}
  in
  let req client =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* () =
      Lsp_helpers.change_config
        ~client
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
        "command": { "command": "", "title": "int -> int" },
        "range": {
          "end": { "character": 19, "line": 4 },
          "start": { "character": 0, "line": 4 }
        }
      },
      {
        "command": { "command": "", "title": "int -> int" },
        "range": {
          "end": { "character": 32, "line": 3 },
          "start": { "character": 0, "line": 3 }
        }
      },
      {
        "command": { "command": "", "title": "nat -> nat" },
        "range": {
          "end": { "character": 68, "line": 2 },
          "start": { "character": 0, "line": 2 }
        }
      }
    ]
    |}]
;;
