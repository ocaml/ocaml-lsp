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

let print_hover_extended resp =
  resp |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover_extended client position verbosity =
  let params =
    let required =
      [ ( "textDocument"
        , TextDocumentIdentifier.yojson_of_t
            (TextDocumentIdentifier.create ~uri:Helpers.uri) )
      ; "position", Position.yojson_of_t position
      ]
    in
    let params =
      match verbosity with
      | None -> required
      | Some v -> ("verbosity", `Int v) :: required
    in
    Some (Jsonrpc.Structured.t_of_yojson (`Assoc params))
  in
  Client.request client (UnknownRequest { meth = "ocamllsp/hoverExtended"; params })
;;

let%expect_test "hover reference" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
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
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
;;

let%expect_test "hover returns type inferred under cursor in a formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "int ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint"
      },
      "range": {
        "end": { "character": 5, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    } |}]
;;

let%expect_test "hover extended" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test ~extra_env:[ "OCAMLLSP_HOVER_IS_EXTENDED=true" ] source req;
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

let%expect_test "default verbosity" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
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
    } |}]
;;

let%expect_test "explicit verbosity 0" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
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
    } |}]
;;

let%expect_test "explicit verbosity 1" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 1) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
;;

let%expect_test "explicit verbosity 2" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 2) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
;;

let%expect_test "implicity verbosity increases" =
  let source = {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml} in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
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
    }
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
;;

let%expect_test "hover extended returns type inferred under cursor in a formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "int ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint"
      },
      "range": {
        "end": { "character": 5, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    } |}]
;;
