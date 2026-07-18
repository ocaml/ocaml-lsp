open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Typed_holes

let typed_holes client =
  Test.custom_request
    client
    Req.meth
    (`Assoc [ "uri", DocumentUri.yojson_of_t Helpers.uri ])
;;

let%expect_test "empty when no holes in file" =
  let source =
    {ocaml|let u = 1
|ocaml}
  in
  let req client =
    let* response = typed_holes client in
    Test.print_result response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| [] |}]
;;

let%expect_test "one hole" =
  let source =
    {ocaml|let k = match () with () -> _
|ocaml}
  in
  let req client =
    let* response = typed_holes client in
    Test.print_result response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "end": { "character": 29, "line": 0 },
        "start": { "character": 28, "line": 0 }
      }
    ]
    |}]
;;

let%expect_test "several holes" =
  let source =
    {ocaml|let u =
  let i = match Some 1 with None -> _ | Some -> _ in
  let b = match () with () -> _ in
  ()
|ocaml}
  in
  let req client =
    let* response = typed_holes client in
    Test.print_result response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    [
      {
        "end": { "character": 31, "line": 2 },
        "start": { "character": 30, "line": 2 }
      },
      {
        "end": { "character": 37, "line": 1 },
        "start": { "character": 36, "line": 1 }
      },
      {
        "end": { "character": 49, "line": 1 },
        "start": { "character": 48, "line": 1 }
      }
    ]
    |}]
;;
