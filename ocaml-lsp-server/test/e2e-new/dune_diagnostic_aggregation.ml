open Test.Import
open Dune_rpc_test

type project =
  { temp : string
  ; root : string
  ; runtime_dir : string
  ; source : string
  ; expected : string
  ; dune_pid : int
  }

let create_project () =
  let temp = Test.temp_dir "ocamllsp-diagnostic-aggregation-" in
  let root = Filename.concat temp "workspace" in
  let runtime_dir = Filename.concat temp "runtime" in
  Unix.mkdir root 0o700;
  Unix.mkdir runtime_dir 0o700;
  Test.write_file (Filename.concat root "dune-project") "(lang dune 3.24)\n";
  let source = Filename.concat root "main.ml" in
  let expected = Filename.concat root "expected.ml" in
  Test.write_file source "let value : string = \"valid\"\n";
  Test.write_file expected "let promoted = 42";
  Test.write_file
    (Filename.concat root "dune")
    {jbuild|
(executable
 (name main))

(alias
 (name repro)
 (deps main.exe))

(rule
 (alias repro)
 (deps expected.ml)
 (target actual.ml)
 (action
  (progn
   (with-stdout-to %{target} (echo "let promoted = 42"))
   (diff expected.ml %{target}))))
|jbuild};
  let dune_pid = start_dune root runtime_dir in
  { temp; root; runtime_dir; source; expected; dune_pid }
;;

let destroy_project project =
  stop_process project.dune_pid;
  ignore (Sys.command ("rm -rf -- " ^ Filename.quote project.temp) : int)
;;

let has_source source (params : PublishDiagnosticsParams.t) =
  List.exists params.diagnostics ~f:(fun (diagnostic : Diagnostic.t) ->
    Option.equal String.equal diagnostic.source (Some source))
;;

let normalize_sandbox string =
  let rec replace = function
    | ".sandbox" :: _hash :: rest -> ".sandbox" :: "<sandbox>" :: replace rest
    | component :: rest -> component :: replace rest
    | [] -> []
  in
  String.split string ~on:'/' |> replace |> String.concat ~sep:"/"
;;

let sanitize_string project string =
  let replacements =
    [ Uri.to_string (Uri.of_path project.source), "<source-uri>"
    ; Uri.to_string (Uri.of_path project.expected), "<expected-uri>"
    ; project.source, "<source-path>"
    ; project.expected, "<expected-path>"
    ; project.root, "<workspace>"
    ; project.temp, "<test-dir>"
    ]
  in
  List.fold_left replacements ~init:string ~f:(fun string (pattern, with_) ->
    String.substr_replace_all string ~pattern ~with_)
  |> normalize_sandbox
;;

let rec sanitize_json project : Yojson.Safe.t -> Yojson.Safe.t = function
  | `String string -> `String (sanitize_string project string)
  | `Assoc fields ->
    `Assoc (List.map fields ~f:(fun (name, value) -> name, sanitize_json project value))
  | `List values -> `List (List.map values ~f:(sanitize_json project))
  | (`Bool _ | `Float _ | `Int _ | `Intlit _ | `Null) as json -> json
;;

let print_publication project label params =
  print_endline label;
  PublishDiagnosticsParams.yojson_of_t params
  |> sanitize_json project
  |> Test.print_result
;;

let rec wait_for_failed_build progress =
  let* (params : Lsp.Progress.t ProgressParams.t) = Mailbox.wait progress in
  match params.value with
  | End { message = Some "Build failed" } -> Fiber.return ()
  | Begin _ | Report _ | End _ -> wait_for_failed_build progress
;;

let capabilities =
  let publishDiagnostics = PublishDiagnosticsClientCapabilities.create () in
  let textDocument = TextDocumentClientCapabilities.create ~publishDiagnostics () in
  let window = WindowClientCapabilities.create ~workDoneProgress:true () in
  ClientCapabilities.create ~textDocument ~window ()
;;

let%expect_test "merge Merlin and Dune diagnostics and honor configuration" =
  let project = create_project () in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       let events = Lifecycle_events.create () in
       run_with_workspace
         ~capabilities
         ~root:project.root
         ~runtime_dir:project.runtime_dir
         events
         ~f:(fun client _workspace ->
           let* () = Signal.wait (Events.dune_ready events.dune) in
           let uri = Uri.of_path project.source in
           let source = "let value : string = 1\n" in
           let* () = open_document client ~uri ~text:source in
           let* merlin =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri uri params
               && has_source "ocamllsp" params
               && Int.equal (List.length params.diagnostics) 1)
           in
           print_publication project "Merlin:" merlin;
           Test.write_file project.source source;
           let* () = wait_for_failed_build (Events.progress events.dune) in
           let* merged =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri uri params
               && has_source "ocamllsp" params
               && Int.equal (List.length params.diagnostics) 1)
           in
           print_publication project "Merlin plus Dune:" merged;
           Test.write_file project.expected "let promoted = 0\n";
           let expected_uri = Uri.of_path project.expected in
           let* dune_only =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri expected_uri params && has_source "dune" params)
           in
           print_publication project "Dune only:" dune_only;
           let settings =
             `Assoc [ "duneDiagnostics", `Assoc [ "enable", `Bool false ] ]
           in
           let* () = Client.notification client (ChangeConfiguration { settings }) in
           let* merlin_only =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri uri params && has_source "ocamllsp" params)
           in
           let+ dune_cleared =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri expected_uri params && List.is_empty params.diagnostics)
           in
           print_publication project "Dune disabled, Merlin:" merlin_only;
           print_publication project "Dune disabled, Dune only:" dune_cleared));
  [%expect
    {|
    Merlin:
    {
      "diagnostics": [
        {
          "message": "The constant 1 has type int but an expression was expected of type string",
          "range": {
            "end": { "character": 22, "line": 0 },
            "start": { "character": 21, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "<source-uri>"
    }
    Merlin plus Dune:
    {
      "diagnostics": [
        {
          "message": "The constant 1 has type int but an expression was expected of type string",
          "range": {
            "end": { "character": 22, "line": 0 },
            "start": { "character": 21, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "<source-uri>"
    }
    Dune only:
    {
      "diagnostics": [
        {
          "message": "diff --git a/_build/default/expected.ml b/_build/.sandbox/<sandbox>/default/actual.ml\nindex 253029d..d7fbee4 100644\n--- a/_build/default/expected.ml\n+++ b/_build/.sandbox/<sandbox>/default/actual.ml\n@@ -1 +1 @@\n-let promoted = 0\n+let promoted = 42\n\\ No newline at end of file",
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ],
      "uri": "<expected-uri>"
    }
    Dune disabled, Merlin:
    {
      "diagnostics": [
        {
          "message": "The constant 1 has type int but an expression was expected of type string",
          "range": {
            "end": { "character": 22, "line": 0 },
            "start": { "character": 21, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        }
      ],
      "uri": "<source-uri>"
    }
    Dune disabled, Dune only:
    { "diagnostics": [], "uri": "<expected-uri>" }
    |}]
;;
