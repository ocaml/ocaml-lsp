open Test.Import
open Dune_rpc_test

let dune_file ~left_ready ~right_ready ~gate =
  Printf.sprintf
    {|
(rule
 (alias repro)
 (deps expected.ml left.ml barrier.sh)
 (target left.actual.ml)
 (action
  (progn
   (run %%{dep:barrier.sh} %S %S %S)
   (copy %%{dep:left.ml} %%{target})
   (diff expected.ml %%{target}))))

(rule
 (alias repro)
 (deps expected.ml right.ml barrier.sh)
 (target right.actual.ml)
 (action
  (progn
   (run %%{dep:barrier.sh} %S %S %S)
   (copy %%{dep:right.ml} %%{target})
   (diff expected.ml %%{target}))))
|}
    left_ready
    right_ready
    gate
    right_ready
    left_ready
    gate
;;

let dune_diagnostics params =
  let params = only_dune_diagnostics params in
  let diagnostics =
    let diagnostic_message (diagnostic : Diagnostic.t) =
      match diagnostic.message with
      | `String message -> message
      | `MarkupContent { value; _ } -> value
    in
    List.sort params.diagnostics ~compare:(fun left right ->
      String.compare (diagnostic_message left) (diagnostic_message right))
  in
  { params with diagnostics }
;;

let dune_diagnostic_count params =
  let params = dune_diagnostics params in
  List.length params.diagnostics
;;

let promotion_actions client uri =
  let params =
    let range =
      let position = Position.create ~line:0 ~character:0 in
      Range.create ~start:position ~end_:position
    in
    let textDocument = TextDocumentIdentifier.create ~uri in
    let context =
      CodeActionContext.create ~diagnostics:[] ~only:[ CodeActionKind.QuickFix ] ()
    in
    CodeActionParams.create ~textDocument ~range ~context ()
  in
  Client.request client (CodeAction params)
  >>| Option.value ~default:[]
  >>| List.filter_map ~f:(function
    | `CodeAction (action : CodeAction.t)
      when Option.exists action.command ~f:(fun command ->
             String.equal command.command "dune/promote") -> Some action
    | `CodeAction _ | `Command _ -> None)
;;

let%expect_test "duplicate shared-source promotions do not hang the server" =
  let project = create_project "shared-promotion" in
  stop_dune project;
  let left_ready = Filename.concat project.temp "left-ready" in
  let right_ready = Filename.concat project.temp "right-ready" in
  let barrier = Filename.concat project.root "barrier.sh" in
  (* Hold both actions until they are running and ocamllsp has subscribed to
     Dune, so both diagnostics reliably exercise the shared source. *)
  Test.write_file
    barrier
    "#!/bin/sh\n\
     set -eu\n\
     touch \"$1\"\n\
     while [ ! -e \"$2\" ]; do sleep 0.01; done\n\
     while [ ! -e \"$3\" ]; do sleep 0.01; done\n";
  Unix.chmod barrier 0o755;
  Test.write_file
    (Filename.concat project.root "dune")
    (dune_file ~left_ready ~right_ready ~gate:project.gate);
  Test.write_file (Filename.concat project.root "left.ml") "let answer = 1\n";
  Test.write_file (Filename.concat project.root "right.ml") "let answer = 2\n";
  let dune_pid = start_dune ~jobs:2 project.root project.runtime_dir in
  Fun.protect
    ~finally:(fun () ->
      stop_process dune_pid;
      destroy_project project)
    (fun () ->
       wait_for_rpc_registration project.runtime_dir dune_pid;
       let events = Lifecycle_events.create () in
       run ~trace:Verbose project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         let* () = Signal.wait (Events.dune_progress events.dune) in
         Test.write_file project.gate "";
         let uri = Uri.of_path project.expected in
         let* diagnostics =
           Events.wait_for_diagnostics events.dune ~f:(fun params ->
             for_uri uri params && dune_diagnostic_count params = 2)
         in
         let* registration = Mailbox.wait events.registrations in
         let* promotions = promotion_actions client uri in
         let registrations = registration :: Mailbox.take_pending events.registrations in
         let errors = Mailbox.take_pending (Events.errors events.dune) in
         let* echo = Client.request client (DebugEcho { message = "still alive" }) in
         print_payload
           project
           "Dune diagnostics:"
           (PublishDiagnosticsParams.yojson_of_t (dune_diagnostics diagnostics));
         print_payloads project "Dune errors:" LogMessageParams.yojson_of_t errors;
         print_payloads
           project
           "registrations:"
           RegistrationParams.yojson_of_t
           registrations;
         print_payloads project "promotions:" CodeAction.yojson_of_t promotions;
         Printf.printf "echo: %s\n" echo.message;
         Fiber.return ()));
  [%expect
    {|
    Dune diagnostics:
    {
      "diagnostics": [
        {
          "message": "--- expected.ml\n+++ left.actual.ml\n@@ -1 +1 @@\n-let answer = 0\n+let answer = 1",
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        },
        {
          "message": "--- expected.ml\n+++ right.actual.ml\n@@ -1 +1 @@\n-let answer = 0\n+let answer = 2",
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
    Dune errors:
    []
    registrations:
    [
      {
        "registrations": [
          {
            "id": "ocamllsp-promote/<document-uri>",
            "method": "textDocument/codeAction",
            "registerOptions": {
              "codeActionKinds": [ "quickfix" ],
              "documentSelector": [
                {
                  "language": null,
                  "scheme": null,
                  "pattern": "<document-path>"
                }
              ]
            }
          }
        ]
      }
    ]
    promotions:
    [
      {
        "command": {
          "arguments": [
            { "dune": "<workspace-path>", "in_source": "<document-path>" }
          ],
          "command": "dune/promote",
          "title": "Promote"
        },
        "kind": "quickfix",
        "title": "Promote"
      }
    ]
    echo: still alive
    |}]
;;
