open Test.Import

let%expect_test "start/stop" =
  ( Test.run @@ fun client ->
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let print_init =
      let+ resp = Client.initialized client in
      print_endline "client: server initialized with:";
      InitializeResult.yojson_of_t resp
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline
    in
    let run =
      let* () = print_init in
      print_endline "client: shutting down server";
      Client.request client Shutdown
    in
    Fiber.fork_and_join_unit run_client (fun () -> run >>> Client.stop client)
  );
  [%expect
    {|
      client: server initialized with:
      {
        "capabilities": {
          "textDocumentSync": {
            "openClose": true,
            "change": 2,
            "willSave": false,
            "willSaveWaitUntil": false,
            "save": true
          },
          "completionProvider": {
            "triggerCharacters": [ ".", "#" ],
            "resolveProvider": true
          },
          "hoverProvider": true,
          "signatureHelpProvider": {
            "triggerCharacters": [ " ", "~", "?", ":", "(" ]
          },
          "declarationProvider": true,
          "definitionProvider": true,
          "typeDefinitionProvider": true,
          "referencesProvider": true,
          "documentHighlightProvider": true,
          "documentSymbolProvider": true,
          "codeActionProvider": {
            "codeActionKinds": [
              "quickfix", "construct", "destruct", "inferred_intf",
              "put module name in identifiers",
              "remove module name from identifiers", "type-annotate"
            ]
          },
          "codeLensProvider": { "resolveProvider": false },
          "documentFormattingProvider": true,
          "renameProvider": { "prepareProvider": true },
          "foldingRangeProvider": true,
          "executeCommandProvider": {
            "commands": [
              "ocamllsp/view-metrics", "ocamllsp/open-related-source",
              "dune/promote"
            ]
          },
          "selectionRangeProvider": true,
          "workspaceSymbolProvider": true,
          "workspace": {
            "workspaceFolders": { "supported": true, "changeNotifications": true }
          },
          "experimental": {
            "ocamllsp": {
              "interfaceSpecificLangId": true,
              "handleSwitchImplIntf": true,
              "handleInferIntf": true,
              "handleTypedHoles": true,
              "handleWrappingAstNode": true,
              "diagnostic_promotions": true
            }
          }
        },
        "serverInfo": { "name": "ocamllsp", "version": "dev" }
      }
      client: shutting down server |}]
