open Test.Import
open Dune_rpc_test

let dune_file =
  {|
(rule
 (alias repro)
 (deps expected.ml left.ml)
 (target left.actual.ml)
 (action
  (progn
   (copy %{dep:left.ml} %{target})
   (diff expected.ml %{target}))))

(rule
 (alias repro)
 (deps expected.ml right.ml)
 (target right.actual.ml)
 (action
  (progn
   (copy %{dep:right.ml} %{target})
   (diff expected.ml %{target}))))
|}
;;

let diagnostic_message (diagnostic : Diagnostic.t) =
  match diagnostic.message with
  | `String message -> message
  | `MarkupContent { value; _ } -> value
;;

let dune_diagnostics params =
  let params = only_dune_diagnostics params in
  let diagnostics =
    List.sort params.diagnostics ~compare:(fun left right ->
      String.compare (diagnostic_message left) (diagnostic_message right))
  in
  { params with diagnostics }
;;

let dune_diagnostic_count params =
  let params = dune_diagnostics params in
  List.length params.diagnostics
;;

let normalize_error (params : LogMessageParams.t) =
  let message =
    if String.is_substring params.message ~substring:"Assert_failure"
    then "promotion registration assertion failed"
    else params.message
  in
  { params with message }
;;

let promotion_actions client uri =
  let position = Position.create ~line:0 ~character:0 in
  let range = Range.create ~start:position ~end_:position in
  let textDocument = TextDocumentIdentifier.create ~uri in
  let context =
    CodeActionContext.create ~diagnostics:[] ~only:[ CodeActionKind.QuickFix ] ()
  in
  let params = CodeActionParams.create ~textDocument ~range ~context () in
  let+ actions = Client.request client (CodeAction params) in
  Option.value actions ~default:[]
  |> List.filter_map ~f:(function
    | `CodeAction (action : CodeAction.t)
      when Option.exists action.command ~f:(fun command ->
             String.equal command.command "dune/promote") -> Some action
    | `CodeAction _ | `Command _ -> None)
;;

let%expect_test "register one promotion for diagnostics sharing a source" =
  let project = create_project "shared-promotion" in
  stop_dune project;
  Test.write_file (Filename.concat project.root "dune") dune_file;
  Test.write_file (Filename.concat project.root "left.ml") "let answer = 1\n";
  Test.write_file (Filename.concat project.root "right.ml") "let answer = 2\n";
  let dune_pid = start_dune project.root project.runtime_dir in
  Fun.protect
    ~finally:(fun () ->
      stop_process dune_pid;
      destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run project events ~f:(fun client _workspace ->
         let* () = Signal.wait (Events.dune_ready events.dune) in
         let uri = Uri.of_path project.expected in
         let* error = Mailbox.wait (Events.errors events.dune) in
         let* () = Lev_fiber.Timer.sleepf 0.05 in
         let diagnostics =
           Events.take_pending events.dune
           |> List.filter ~f:(for_uri uri)
           |> List.map ~f:dune_diagnostics
           |> List.filter ~f:(fun (params : PublishDiagnosticsParams.t) ->
             not (List.is_empty params.diagnostics))
         in
         let errors =
           error :: Mailbox.take_pending (Events.errors events.dune)
           |> List.map ~f:normalize_error
         in
         let registrations = Mailbox.take_pending events.registrations in
         let* promotions = promotion_actions client uri in
         let* echo = Client.request client (DebugEcho { message = "still alive" }) in
         print_payloads
           project
           "Dune diagnostics:"
           PublishDiagnosticsParams.yojson_of_t
           diagnostics;
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
    []
    Dune errors:
    [
      { "message": "promotion registration assertion failed", "type": 1 }
    ]
    registrations:
    []
    promotions:
    []
    echo: still alive
    |}]
;;
