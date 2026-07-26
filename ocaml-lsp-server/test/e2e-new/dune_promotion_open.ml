open Test.Import
open Dune_rpc_test

let%expect_test "promotion removal while its document is open" =
  let project = create_project "po" in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         Test.write_file project.gate "";
         let uri = Uri.of_path project.expected in
         let* initial = Events.wait_for_diagnostics events.dune ~f:has_dune_diagnostic in
         let* registration = Mailbox.wait events.registrations in
         let* () = open_document client ~uri ~text:project.old_source in
         let* opened_unregistration = Mailbox.wait events.unregistrations in
         Test.write_file project.expected "let answer = 42";
         Test.write_file project.trigger "fixed\n";
         let* cleared =
           Events.wait_for_diagnostics events.dune ~f:(fun params ->
             for_uri initial.uri params && no_dune_diagnostic params)
         in
         let* () = Lev_fiber.Timer.sleepf 0.01 in
         print_payload
           project
           "initial textDocument/publishDiagnostics:"
           (PublishDiagnosticsParams.yojson_of_t initial);
         print_payload
           project
           "initial client/registerCapability:"
           (RegistrationParams.yojson_of_t registration);
         print_payload
           project
           "client/unregisterCapability after textDocument/didOpen:"
           (UnregistrationParams.yojson_of_t opened_unregistration);
         print_payload
           project
           "Dune diagnostics after removing the promotion:"
           (PublishDiagnosticsParams.yojson_of_t (only_dune_diagnostics cleared));
         print_payloads
           project
           "client/unregisterCapability after removing the promotion:"
           UnregistrationParams.yojson_of_t
           (Mailbox.take_pending events.unregistrations);
         Fiber.return ()));
  [%expect
    {|
    initial textDocument/publishDiagnostics:
    {
      "diagnostics": [
        {
          "message": "--- expected.ml\n+++ actual.ml\n@@ -1 +1 @@\n-let answer = 0\n+let answer = 42\n\\ No newline at end of file",
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ],
      "uri": "<document-uri>"
    }
    initial client/registerCapability:
    {
      "registrations": [
        {
          "id": "ocamllsp-promote/<document-uri>",
          "method": "textDocument/codeAction",
          "registerOptions": {
            "codeActionKinds": [ "Promote" ],
            "documentSelector": [
              { "language": null, "scheme": null, "pattern": "<document-path>" }
            ]
          }
        }
      ]
    }
    client/unregisterCapability after textDocument/didOpen:
    {
      "unregisterations": [
        {
          "id": "ocamllsp-promote/<document-uri>",
          "method": "textDocument/codeAction"
        }
      ]
    }
    Dune diagnostics after removing the promotion:
    { "diagnostics": [], "uri": "<document-uri>" }
    client/unregisterCapability after removing the promotion:
    [
      {
        "unregisterations": [
          {
            "id": "ocamllsp-promote/<document-uri>",
            "method": "textDocument/codeAction"
          }
        ]
      }
    ]
    |}]
;;
