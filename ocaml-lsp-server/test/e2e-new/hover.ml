open Test.Import

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let%expect_test "object method call" =
  let source =
    {ocaml|
let f (o : <  g : int -> unit >) = o#g 4
|ocaml}
  in
  let position = Position.create ~line:1 ~character:38 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int -> unit" },
      "range": {
        "end": { "character": 38, "line": 1 },
        "start": { "character": 35, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "hover over warning attribute" =
  let source =
    {ocaml|
let () =
  let x [@warning "-27+4..6@a"] = 1 in
  ()
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": "Disables warning 27: Innocuous unused variable: unused variable that is not bound with\n    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n    character.\nEnables warnings 4 to 6\nEnables warning set 'a' as errors",
      "range": {
        "end": { "character": 30, "line": 2 },
        "start": { "character": 18, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "hover over warning attribute comprehensive" =
  let source =
    {ocaml|
let () =
  let x [@warning "+9-26@3+10..15-20..25@30..35+a-z@b"] = 1 in
  ()
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": "Enables warning 9: Missing fields in a record pattern.\nDisables warning 26: Suspicious unused variable: unused variable that is bound\n    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n    character.\nEnables warning 3 as an error: Deprecated synonym for the 'deprecated' alert.\nEnables warnings 10 to 15\nDisables warnings 20 to 25\nEnables warnings 30 to 35 as errors\nEnables warning set 'a'\nDisables warning set 'z'\nEnables warning set 'b' as errors",
      "range": {
        "end": { "character": 54, "line": 2 },
        "start": { "character": 18, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "hover over warning attribute invalid formats" =
  let source =
    {ocaml|
let () =
  let x [@warning "+-@++1..+ab"] = 1 in
  ()
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| no hover response |}]
;;
