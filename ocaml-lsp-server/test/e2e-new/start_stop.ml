open Test.Import

let%expect_test "start/stop" =
  ( Test.run @@ fun client ->
    let run_client () =
      let capabilities =
        let window =
          let showDocument =
            ShowDocumentClientCapabilities.create ~support:true
          in
          WindowClientCapabilities.create ~showDocument ()
        in
        ClientCapabilities.create ~window ()
      in
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
          "codeActionProvider": {
            "codeActionKinds": [
              "quickfix", "construct", "destruct", "inferred_intf",
              "put module name in identifiers",
              "remove module name from identifiers", "type-annotate"
            ]
          },
          "codeLensProvider": { "resolveProvider": false },
          "completionProvider": {
            "resolveProvider": true,
            "triggerCharacters": [ ".", "#" ]
          },
          "declarationProvider": true,
          "definitionProvider": true,
          "documentFormattingProvider": true,
          "documentHighlightProvider": true,
          "documentSymbolProvider": true,
          "executeCommandProvider": {
            "commands": [
              "ocamllsp/view-metrics", "ocamllsp/open-related-source",
              "dune/promote"
            ]
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
          },
          "foldingRangeProvider": true,
          "hoverProvider": true,
          "referencesProvider": true,
          "renameProvider": { "prepareProvider": true },
          "selectionRangeProvider": true,
          "signatureHelpProvider": {
            "triggerCharacters": [ " ", "~", "?", ":", "(" ]
          },
          "textDocumentSync": {
            "change": 2,
            "openClose": true,
            "save": true,
            "willSave": false,
            "willSaveWaitUntil": false
          },
          "typeDefinitionProvider": true,
          "workspace": {
            "workspaceFolders": { "changeNotifications": true, "supported": true }
          },
          "workspaceSymbolProvider": true
        },
        "serverInfo": { "name": "ocamllsp", "version": "dev" }
      }
      client: shutting down server |}]
