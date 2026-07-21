open Test.Import
open Dune_rpc_test

let%expect_test "connected Dune does not process workspace removal" =
  let project = create_project "ws" in
  let events = Lifecycle_events.create () in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       run project events ~f:(fun client workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         Test.write_file project.gate "";
         let* initial = Events.wait_for_diagnostics events.dune ~f:has_dune_diagnostic in
         let* registration = Mailbox.wait events.registrations in
         print_payload
           project
           "initial textDocument/publishDiagnostics:"
           (PublishDiagnosticsParams.yojson_of_t initial);
         print_payload
           project
           "initial client/registerCapability:"
           (RegistrationParams.yojson_of_t registration);
         let event =
           WorkspaceFoldersChangeEvent.create ~added:[] ~removed:[ workspace ]
         in
         let params = DidChangeWorkspaceFoldersParams.create ~event in
         print_payload
           project
           "client workspace/didChangeWorkspaceFolders:"
           (DidChangeWorkspaceFoldersParams.yojson_of_t params);
         let* () = Client.notification client (ChangeWorkspaceFolders params) in
         let* () = Lev_fiber.Timer.sleepf 0.1 in
         print_payloads
           project
           "textDocument/publishDiagnostics after workspace removal:"
           PublishDiagnosticsParams.yojson_of_t
           (Events.take_pending events.dune);
         print_payloads
           project
           "client/unregisterCapability after workspace removal:"
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
    client workspace/didChangeWorkspaceFolders:
    {
      "event": {
        "added": [],
        "removed": [ { "name": "dune-rpc", "uri": "<workspace-uri>" } ]
      }
    }
    textDocument/publishDiagnostics after workspace removal:
    []
    client/unregisterCapability after workspace removal:
    []
    |}]
;;
