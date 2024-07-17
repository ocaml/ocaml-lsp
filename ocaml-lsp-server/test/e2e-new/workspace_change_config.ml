open Test.Import

let change_config client params = Client.notification client (ChangeConfiguration params)

let codelens client textDocument =
  Client.request
    client
    (TextDocumentCodeLens
       { textDocument; workDoneToken = None; partialResultToken = None })
;;

let%expect_test "disable codelens" =
  let source = {ocaml|
let string = "Hello"
|ocaml} in
  let req client =
    let text_document = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let* () =
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "codelens", `Assoc [ "enable", `Bool false ] ]))
    in
    let* resp_codelens_disabled = codelens client text_document in
    print_endline ("CodeLens found: " ^ string_of_int (List.length resp_codelens_disabled));
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| CodeLens found: 0 |}]
;;

let%expect_test "enable hover extended" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = Hover_extended.hover client position in
    let () = Hover_extended.print_hover resp in
    let* () =
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:(`Assoc [ "extendedHover", `Assoc [ "enable", `Bool true ] ]))
    in
    (* The first hover request has verbosity = 0 *)
    let* _ = Hover_extended.hover client position in
    (* The second hover request has verbosity = 1 *)
    let* resp = Hover_extended.hover client position in
    let () = Hover_extended.print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
;;
