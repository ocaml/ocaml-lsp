open Test.Import
open Dune_rpc_test

let client_capabilities =
  let diagnostic = DiagnosticClientCapabilities.create () in
  let textDocument = TextDocumentClientCapabilities.create ~diagnostic () in
  ClientCapabilities.create ~textDocument ()
;;

let rec wait_for_failed_build progress =
  let* (params : Lsp.Progress.t ProgressParams.t) = Mailbox.wait progress in
  match params.value with
  | End { message = Some "Build failed" } -> Fiber.return ()
  | Begin _ | Report _ | End _ -> wait_for_failed_build progress
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
     Test.open_document
       ~client
       ~uri
       ~source:"let x : int = \"only Merlin sees this\"\n"
       ()
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

let%expect_test "pulls Dune document diagnostics" =
  let project = create_project "pull-diagnostics" in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       let window = WindowClientCapabilities.create ~workDoneProgress:true () in
       let capabilities = { client_capabilities with window = Some window } in
       run ~capabilities project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         let uri = Uri.of_path project.expected in
         let* () = open_document client ~uri ~text:project.old_source in
         Test.write_file project.gate "";
         let* () = wait_for_failed_build (Events.progress events.dune) in
         let+ report = pull_document_diagnostics client uri None in
         match report with
         | `RelatedFullDocumentDiagnosticReport report ->
           let dune =
             List.count report.items ~f:(fun (diagnostic : Diagnostic.t) ->
               Option.equal String.equal diagnostic.source (Some "dune"))
           in
           Printf.printf "full items=%d dune=%d\n" (List.length report.items) dune
         | `RelatedUnchangedDocumentDiagnosticReport _ ->
           failwith "expected full document diagnostics"));
  [%expect {| full items=1 dune=1 |}]
;;
