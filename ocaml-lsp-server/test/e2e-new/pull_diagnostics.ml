open Test.Import

let client_capabilities =
  let diagnostic = DiagnosticClientCapabilities.create () in
  let textDocument = TextDocumentClientCapabilities.create ~diagnostic () in
  ClientCapabilities.create ~textDocument ()
;;

let pull_document_diagnostics client uri previousResultId =
  let textDocument = TextDocumentIdentifier.create ~uri in
  let params =
    DocumentDiagnosticParams.create
      ~identifier:"ocamllsp"
      ?previousResultId
      ~textDocument
      ()
  in
  Client.request client (TextDocumentDiagnostic params)
;;

let%expect_test "advertises document pull diagnostics" =
  (Test.run_initialized ~capabilities:client_capabilities
   @@ fun client ->
   let* initialized = Client.initialized client in
   (match initialized.capabilities.diagnosticProvider with
    | Some (`DiagnosticOptions options) ->
      DiagnosticOptions.yojson_of_t options |> Test.print_result
    | Some (`DiagnosticRegistrationOptions _) | None -> assert false);
   Client.stop client);
  [%expect
    {|
    {
      "identifier": "ocamllsp",
      "interFileDependencies": true,
      "workspaceDiagnostics": false
    }
    |}]
;;

let%expect_test "pulls Merlin document diagnostics" =
  (Test.run_initialized ~capabilities:client_capabilities
   @@ fun client ->
   let uri = Helpers.uri in
   let* () =
     Test.openDocument ~client ~uri ~source:"let x : int = \"only Merlin sees this\"\n"
   in
   let* first = pull_document_diagnostics client uri None in
   let result_id =
     match first with
     | `RelatedFullDocumentDiagnosticReport report ->
       let has_merlin =
         List.exists report.items ~f:(fun diagnostic ->
           Option.equal String.equal diagnostic.Diagnostic.source (Some "ocamllsp"))
       in
       Printf.printf "full items=%d merlin=%b\n" (List.length report.items) has_merlin;
       Option.value_exn report.resultId
     | `RelatedUnchangedDocumentDiagnosticReport _ ->
       failwith "expected full document diagnostics"
   in
   let* unchanged = pull_document_diagnostics client uri (Some result_id) in
   (match unchanged with
    | `RelatedUnchangedDocumentDiagnosticReport _ -> print_endline "unchanged"
    | `RelatedFullDocumentDiagnosticReport _ ->
      failwith "expected unchanged document diagnostics");
   Client.stop client);
  [%expect
    {|
    full items=1 merlin=true
    unchanged |}]
;;
