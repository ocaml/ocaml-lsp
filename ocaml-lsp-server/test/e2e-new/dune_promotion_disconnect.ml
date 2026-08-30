open Test.Import
open Dune_rpc_test

let%expect_test "promotion registrations when Dune disconnects" =
  let project = create_project "pd" in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run project events ~f:(fun _client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         Test.write_file project.gate "";
         let* initial = Events.wait_for_diagnostics events.dune ~f:has_dune_diagnostic in
         let* registration = Mailbox.wait events.registrations in
         let promotion_cleanup = Mailbox.wait events.unregistrations in
         stop_dune project;
         let* cleared =
           Events.wait_for_diagnostics events.dune ~f:(fun params ->
             for_uri initial.uri params && no_dune_diagnostic params)
         in
         let* unregistration = promotion_cleanup in
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
           "textDocument/publishDiagnostics after Dune disconnects:"
           (PublishDiagnosticsParams.yojson_of_t cleared);
         print_payloads
           project
           "client/unregisterCapability after Dune disconnects:"
           UnregistrationParams.yojson_of_t
           (unregistration :: Mailbox.take_pending events.unregistrations);
         restart_dune project;
         let* () = Signal.wait (Events.dune_ready events.dune) in
         let* reconnected =
           Events.wait_for_diagnostics events.dune ~f:has_dune_diagnostic
         in
         let* reregistration = Mailbox.wait events.registrations in
         print_payload
           project
           "textDocument/publishDiagnostics after Dune reconnects:"
           (PublishDiagnosticsParams.yojson_of_t reconnected);
         print_payload
           project
           "client/registerCapability after Dune reconnects:"
           (RegistrationParams.yojson_of_t reregistration);
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
            "codeActionKinds": [ "quickfix" ],
            "documentSelector": [
              { "language": null, "scheme": null, "pattern": "<document-path>" }
            ]
          }
        }
      ]
    }
    textDocument/publishDiagnostics after Dune disconnects:
    { "diagnostics": [], "uri": "<document-uri>" }
    client/unregisterCapability after Dune disconnects:
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
    textDocument/publishDiagnostics after Dune reconnects:
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
    client/registerCapability after Dune reconnects:
    {
      "registrations": [
        {
          "id": "ocamllsp-promote/<document-uri>",
          "method": "textDocument/codeAction",
          "registerOptions": {
            "codeActionKinds": [ "quickfix" ],
            "documentSelector": [
              { "language": null, "scheme": null, "pattern": "<document-path>" }
            ]
          }
        }
      ]
    }
    |}]
;;
