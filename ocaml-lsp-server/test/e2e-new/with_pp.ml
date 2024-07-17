open! Test.Import

let path = Filename.concat (Sys.getcwd ()) "for_pp.ml"
let uri = DocumentUri.of_path path

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

let%expect_test "with-pp" =
  let position = Position.create ~line:0 ~character:9 in
  let handler =
    Client.Handler.make
      ~on_notification:(fun client _notification ->
        Client.state client;
        Fiber.return ())
      ()
  in
  let output =
    Test.run ~handler
    @@ fun client ->
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        let text = Io.String_path.read_file path in
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () =
        let+ resp = hover_req client position in
        print_hover resp
      in
      let output = [%expect.output] in
      let* () = Client.request client Shutdown in
      let+ () = Client.stop client in
      output
    in
    Fiber.fork_and_join_unit run_client run
  in
  let (_ : string) = [%expect.output] in
  print_endline output;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "type universe" },
      "range": {
        "end": { "character": 13, "line": 0 },
        "start": { "character": 0, "line": 0 }
      }
    }|}]
;;
