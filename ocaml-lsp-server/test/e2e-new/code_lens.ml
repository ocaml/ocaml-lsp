open Test.Import

let change_config client params = Client.notification client (ChangeConfiguration params)

let codelens client textDocument =
  Client.request
    client
    (TextDocumentCodeLens
       { textDocument; workDoneToken = None; partialResultToken = None })
;;

let%expect_test "enable only codelens for toplevel let binding 1" =
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
           ~settings:(`Assoc [ "codelens", `Assoc [ "only_toplevel", `Bool true ] ]))
    in
    let* resp_codelens_toplevel = codelens client text_document in
    print_endline ("CodeLens found: " ^ string_of_int (List.length resp_codelens_toplevel));
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| CodeLens found: 3 |}]
;;

let%expect_test "enable only codelens for toplevel let binding 2" =
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
           ~settings:(`Assoc [ "codelens", `Assoc [ "only_toplevel", `Bool true ] ]))
    in
    let* resp_codelens_toplevel = codelens client text_document in
    print_endline ("CodeLens found: " ^ string_of_int (List.length resp_codelens_toplevel));
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| CodeLens found: 1 |}]
;;
