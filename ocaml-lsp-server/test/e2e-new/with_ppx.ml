open! Test.Import

let path = Filename.concat (Sys.getcwd ()) "for_ppx.ml"
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

let%expect_test "with-ppx" =
  (* We will call 'hover' on the last line of this very file *)
  let position = Position.create ~line:2 ~character:5 in
  (* We need to wait for the first diagnostics *)
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    let on_notification (_ : _ Client.t) (n : Client.in_notification) =
      match n with
      | PublishDiagnostics diag ->
        printfn "Received %i diagnostics" (List.length diag.diagnostics);
        List.iter diag.diagnostics ~f:(fun (d : Diagnostic.t) ->
          match d.message with
          | `String m -> print_endline m
          | `MarkupContent _ -> assert false);
        Fiber.Ivar.fill diagnostics ()
      | _ -> Fiber.return ()
    in
    Client.Handler.make ~on_notification ()
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
      let* () = Fiber.Ivar.read diagnostics in
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
    {xxx|
    Received 0 diagnostics
    {
      "contents": {
        "value": "(* ppx expect expansion *)\nExpect_test_collector.Current_file.unset ()",
        "language": "ocaml"
      },
      "range": {
        "end": { "character": 16, "line": 2 },
        "start": { "character": 4, "line": 2 }
      }
    }
    |xxx}]
;;
