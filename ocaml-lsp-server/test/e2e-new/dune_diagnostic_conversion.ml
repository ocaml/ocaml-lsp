open Test.Import
open Dune_rpc_test

let capabilities ~promotion_data =
  let publishDiagnostics =
    PublishDiagnosticsClientCapabilities.create
      ~relatedInformation:true
      ~dataSupport:true
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~publishDiagnostics () in
  let window = WindowClientCapabilities.create ~workDoneProgress:true () in
  let experimental =
    if promotion_data then Some (`Assoc [ "diagnostic_promotions", `Bool true ]) else None
  in
  ClientCapabilities.create ~textDocument ~window ?experimental ()
;;

let dune_diagnostic (params : PublishDiagnosticsParams.t) =
  match
    List.filter params.diagnostics ~f:(fun (diagnostic : Diagnostic.t) ->
      Option.equal String.equal diagnostic.source (Some "dune"))
  with
  | [ diagnostic ] -> diagnostic
  | diagnostics ->
    failwith
      (Printf.sprintf "expected one Dune diagnostic, got %d" (List.length diagnostics))
;;

let print_diagnostic project ~promotion_data params =
  let diagnostic = dune_diagnostic params in
  Printf.printf
    "promotion data enabled: %b\nseverity: %s\n"
    promotion_data
    (match diagnostic.severity with
     | Some Error -> "error"
     | Some Warning -> "warning"
     | Some Information -> "information"
     | Some Hint -> "hint"
     | None -> "none");
  print_payload project "range:" (Range.yojson_of_t diagnostic.range);
  match diagnostic.data with
  | None -> print_endline "promotion data: none"
  | Some data -> print_payload project "promotion data:" data
;;

let run_case ~promotion_data =
  let project = create_project (if promotion_data then "data" else "no-data") in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run
         ~capabilities:(capabilities ~promotion_data)
         project
         events
         ~f:(fun _client _workspace ->
           let* () = Signal.wait (Events.dune_ready events.dune) in
           Test.write_file project.gate "";
           let+ params =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri (Uri.of_path project.expected) params && has_dune_diagnostic params)
           in
           print_diagnostic project ~promotion_data params))
;;

let%expect_test "convert promotion diagnostics produced by Dune" =
  run_case ~promotion_data:false;
  run_case ~promotion_data:true;
  [%expect
    {|
    promotion data enabled: false
    severity: error
    range:
    {
      "end": { "character": 0, "line": 0 },
      "start": { "character": 0, "line": 0 }
    }
    promotion data: none
    promotion data enabled: true
    severity: error
    range:
    {
      "end": { "character": 0, "line": 0 },
      "start": { "character": 0, "line": 0 }
    }
    promotion data:
    {
      "diagnostic_promotions": [
        {
          "in_build": "<workspace-path>/_build/.promotion-staging/expected.ml",
          "in_source": "<document-path>"
        }
      ]
    }
    |}]
;;
