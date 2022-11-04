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

let print_hover_extended resp =
  resp |> Yojson.Safe.pretty_to_string ~std:false |> print_endline

let hover_extended client position verbosity =
  let params =
    let required =
      [ ( "textDocument"
        , TextDocumentIdentifier.yojson_of_t
            (TextDocumentIdentifier.create ~uri) )
      ; ("position", Position.yojson_of_t position)
      ]
    in
    let params =
      match verbosity with
      | None -> required
      | Some v -> ("verbosity", `Int v) :: required
    in
    Some (Jsonrpc.Structured.t_of_yojson (`Assoc params))
  in
  Client.request
    client
    (UnknownRequest { meth = "ocamllsp/hoverExtended"; params })

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

let%expect_test "hover extended" =
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
  test ~extra_env:[ "OCAMLLSP_HOVER_IS_EXTENDED=true" ] source req;
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
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
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
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
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
    let* resp = hover_extended client position (Some 1) in
    let () = print_hover_extended resp in
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
    let* resp = hover_extended client position (Some 2) in
    let () = print_hover_extended resp in
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
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
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
