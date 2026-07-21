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
         stop_dune project;
         let* cleared =
           Events.wait_for_diagnostics events.dune ~f:(fun params ->
             for_uri initial.uri params && no_dune_diagnostic params)
         in
         let* () = Lev_fiber.Timer.sleepf 0.02 in
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
    []
    |}]
;;
