open! Test.Import

let change_config client params = Client.notification client (ChangeConfiguration params)
let uri = DocumentUri.of_path "test.ml"
let create_postion line character = Position.create ~line ~character

let activate_syntax_doc =
  DidChangeConfigurationParams.create
    ~settings:(`Assoc [ "syntaxDocumentation", `Assoc [ "enable", `Bool true ] ])
;;

let deactivate_syntax_doc =
  DidChangeConfigurationParams.create
    ~settings:(`Assoc [ "syntaxDocumentation", `Assoc [ "enable", `Bool false ] ])
;;

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover_req client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri
       ; workDoneToken = None
       })
;;

let run_test text req =
  let handler =
    Client.Handler.make
      ~on_notification:(fun client _notification ->
        Client.state client;
        Fiber.return ())
      ()
  in
  Test.run ~handler (fun client ->
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = req client in
      let* () = Client.request client Shutdown in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run)
;;

let%expect_test "syntax doc should display" =
  let source = {ocaml|
type color = Red|Blue
|ocaml} in
  let position = create_postion 1 9 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let (_ : string) = [%expect.output] in
  run_test source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "type color = Red | Blue\n***\n`syntax` Variant Type: Represent's data that may take on multiple different forms.. See [Manual](https://v2.ocaml.org/releases/4.14/htmlman/typedecl.html#ss:typedefs)"
      },
      "range": {
        "end": { "character": 21, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    } |}]
;;

let%expect_test "syntax doc should not display" =
  let source = {ocaml|
type color = Red|Blue
|ocaml} in
  let position = create_postion 1 9 in
  let req client =
    let* () = change_config client deactivate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  run_test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "type color = Red | Blue" },
      "range": {
        "end": { "character": 21, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    } |}]
;;

let%expect_test "syntax doc should print" =
  let source = {ocaml|
type t = ..
|ocaml} in
  let position = create_postion 1 5 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  run_test source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "type t = ..\n***\n`syntax` Extensible Variant Type: Can be extended with new variant constructors using `+=`.. See [Manual](https://v2.ocaml.org/releases/4.14/htmlman/extensiblevariants.html)"
      },
      "range": {
        "end": { "character": 11, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    } |}]
;;

let%expect_test "should receive no hover response" =
  let source = {ocaml|
  let a = 1
  |ocaml} in
  let position = create_postion 1 5 in
  let req client =
    let* () = change_config client activate_syntax_doc in
    let* resp = hover_req client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  run_test source req;
  [%expect {| no hover response |}]
;;
