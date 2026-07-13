open Test.Import

let capabilities =
  let parameterInformation =
    SignatureHelpClientCapabilities.create_parameterInformation
      ~labelOffsetSupport:true
      ()
  in
  let signatureInformation =
    SignatureHelpClientCapabilities.create_signatureInformation
      ~documentationFormat:[ MarkupKind.Markdown; MarkupKind.PlainText ]
      ~parameterInformation
      ()
  in
  let signatureHelp =
    SignatureHelpClientCapabilities.create
      ~dynamicRegistration:true
      ~signatureInformation
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~signatureHelp () in
  ClientCapabilities.create ~textDocument ()
;;

let signature_help client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (SignatureHelp (SignatureHelpParams.create ~textDocument ~position ()))
;;

let print_signature_help signature_help =
  signature_help |> SignatureHelp.yojson_of_t |> Test.print_result
;;

let test source position =
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run ~handler (fun client ->
    let run_client () = Client.start client (InitializeParams.create ~capabilities ()) in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        TextDocumentItem.create
          ~uri:Helpers.uri
          ~languageId:"ocaml"
          ~version:0
          ~text:source
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* response = signature_help client position in
      print_signature_help response;
      let* () = Client.request client Shutdown in
      let* () = Fiber.Ivar.read diagnostics in
      Client.stop client
    in
    Fiber.fork_and_join_unit run_client run)
;;

let%expect_test "can provide signature help after a function-type value" =
  let source =
    {ocaml|let map = ListLabels.map

let _ = map
|ocaml}
  in
  test source (Position.create ~line:2 ~character:11);
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "map : f:('a -> 'b) -> 'a list -> 'b list",
          "parameters": [ { "label": [ 6, 18 ] }, { "label": [ 22, 29 ] } ]
        }
      ]
    }
    |}]
;;
