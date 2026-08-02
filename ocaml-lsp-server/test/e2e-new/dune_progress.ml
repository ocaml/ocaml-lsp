open Test.Import
open Dune_rpc_test

let normalize_error (params : LogMessageParams.t) =
  let message =
    if
      String.is_substring
        params.message
        ~substring:"Invalid_argument(\"progress reporting is not supported\")"
    then "Invalid_argument(\"progress reporting is not supported\")"
    else params.message
  in
  { params with message }
;;

let%expect_test "Dune remains connected when work-done progress is unsupported" =
  let project = create_project "no-progress" in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       let capabilities = ClientCapabilities.create () in
       run ~capabilities ~trace:Verbose project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         (* Synchronize with a real Dune progress update before triggering the
            build result used by the rest of the assertion. *)
         let* () = Signal.wait (Events.dune_progress events.dune) in
         Test.write_file project.gate "";
         let* diagnostic =
           Events.wait_for_diagnostics events.dune ~f:has_dune_diagnostic
         in
         let* echo = Client.request client (DebugEcho { message = "still connected" }) in
         let progress_creations = Mailbox.take_pending events.progress_creations in
         let progress_notifications =
           Mailbox.take_pending (Events.progress events.dune)
         in
         stop_dune project;
         let* cleared =
           Events.wait_for_diagnostics events.dune ~f:(fun params ->
             for_uri diagnostic.uri params && no_dune_diagnostic params)
         in
         let* () = Lev_fiber.Timer.sleepf 0.05 in
         let errors =
           Mailbox.take_pending (Events.errors events.dune) |> List.map ~f:normalize_error
         in
         print_payload
           project
           "Dune diagnostics:"
           (PublishDiagnosticsParams.yojson_of_t diagnostic);
         print_payload
           project
           "Dune diagnostics after disconnect:"
           (PublishDiagnosticsParams.yojson_of_t cleared);
         print_payloads project "Dune errors:" LogMessageParams.yojson_of_t errors;
         print_payloads
           project
           "progress creations:"
           WorkDoneProgressCreateParams.yojson_of_t
           progress_creations;
         print_payloads
           project
           "progress notifications:"
           (ProgressParams.yojson_of_t Lsp.Progress.yojson_of_t)
           progress_notifications;
         Printf.printf "echo: %s\n" echo.message;
         Fiber.return ()));
  [%expect
    {|
    Dune diagnostics:
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
    Dune diagnostics after disconnect:
    { "diagnostics": [], "uri": "<document-uri>" }
    Dune errors:
    []
    progress creations:
    []
    progress notifications:
    []
    echo: still connected
    |}]
;;
