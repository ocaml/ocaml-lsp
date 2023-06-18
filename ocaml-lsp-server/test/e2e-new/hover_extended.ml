open Test.Import

let client_capabilities = ClientCapabilities.create ()

let uri = DocumentUri.of_path "test.ml"

let test ?extra_env text req =
  let handler =
    Client.Handler.make
      ~on_notification:(fun client _notification ->
        Client.state client;
        Fiber.return ())
      ()
  in
  Test.run ~handler ?extra_env (fun client ->
      let run_client () =
        Client.start
          client
          (InitializeParams.create ~capabilities:client_capabilities ())
      in
      let run () =
        let* (_ : InitializeResult.t) = Client.initialized client in
        let textDocument =
          TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
        in
        let* () =
          Client.notification
            client
            (TextDocumentDidOpen
               (DidOpenTextDocumentParams.create ~textDocument))
        in
        let* () = req client in
        let* () = Client.request client Shutdown in
        Client.stop client
      in
      Fiber.fork_and_join_unit run_client run)

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t
    |> Yojson.Safe.pretty_to_string ~std:false
    |> print_endline

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri
       ; workDoneToken = None
       })

let change_config_hover client verbosity =
  let required = [ ("enable", `Bool true) ] in

  let params =
    match verbosity with
    | None -> required
    | Some verbosity -> ("verbosity", `Int verbosity) :: required
  in

  let settings = `Assoc [ ("extendedHover", `Assoc params) ] in

  Client.notification
    client
    (ChangeConfiguration (DidChangeConfigurationParams.create ~settings))

let%expect_test "hover reference" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
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
  test source req;
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

let%expect_test "hover extended" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* () = change_config_hover client None in
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
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

let%expect_test "default verbosity" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* () = change_config_hover client None in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]

let%expect_test "explicit verbosity 0" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* () = change_config_hover client (Some 0) in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]

let%expect_test "explicit verbosity 1" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* () = change_config_hover client (Some 1) in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]

let%expect_test "explicit verbosity 2" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* () = change_config_hover client (Some 2) in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]

let%expect_test "implicity verbosity increases" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* () = change_config_hover client None in
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
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

let%expect_test "hover extended returns type inferred under cursor in a \
                 formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* () = change_config_hover client None in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  test source req;
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
