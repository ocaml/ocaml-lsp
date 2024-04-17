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

let change_config client params =
  Client.notification client (ChangeConfiguration params)

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri
       ; workDoneToken = None
       })

let%expect_test "supports changing Dune context configuration" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* () =
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:
             (`Assoc [ ("duneContext", `Assoc [ ("value", `String "alt") ]) ]))
    in
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
