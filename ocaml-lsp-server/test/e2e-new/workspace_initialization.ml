open Test.Import
open Dune_rpc_test

let diagnostic_shape (params : PublishDiagnosticsParams.t) =
  let params = only_dune_diagnostics params in
  let diagnostics =
    List.map params.diagnostics ~f:(fun (diagnostic : Diagnostic.t) ->
      `Assoc
        [ "range", Range.yojson_of_t diagnostic.range
        ; ( "severity"
          , Option.value_map
              diagnostic.severity
              ~default:`Null
              ~f:DiagnosticSeverity.yojson_of_t )
        ; ( "source"
          , Option.value_map diagnostic.source ~default:`Null ~f:(fun x -> `String x) )
        ])
  in
  `Assoc [ "uri", Uri.yojson_of_t params.uri; "diagnostics", `List diagnostics ]
;;

let run_case
      ?cwd
      ?rootPath
      ?rootUri
      ?workspaceFolders
      (project : Dune_rpc_test.project)
      label
  =
  let events = Lifecycle_events.create () in
  Test.run_initialized
    ?cwd
    ~extra_env:[ "OCAMLLSP_TEST=false"; "XDG_RUNTIME_DIR=" ^ project.runtime_dir ]
    ~timeout:30.0
    ~handler:(Lifecycle_events.handler events)
    ?rootPath
    ?rootUri
    ?workspaceFolders
  @@ fun client ->
  let* () = Signal.wait (Events.dune_ready events.dune) in
  Test.write_file project.gate "";
  let* params =
    Events.wait_for_diagnostics events.dune ~f:(fun params ->
      for_uri (Uri.of_path project.expected) params && has_dune_diagnostic params)
  in
  print_payload project (label ^ ":") (diagnostic_shape params);
  Test.shutdown_client client
;;

let with_project name f =
  let project = create_project name in
  Fun.protect ~finally:(fun () -> destroy_project project) (fun () -> f project)
;;

let other_root (project : Dune_rpc_test.project) name =
  let path = Filename.concat project.temp name in
  Unix.mkdir path 0o700;
  path
;;

let%expect_test "workspace initialization precedence and fallbacks" =
  with_project "workspace-precedence" (fun project ->
    let workspace =
      WorkspaceFolder.create ~uri:(Uri.of_path project.root) ~name:"workspace-folder"
    in
    let other = other_root project "other-root" in
    run_case
      project
      "workspaceFolders over roots"
      ~workspaceFolders:(Some [ workspace ])
      ~rootUri:(Uri.of_path other)
      ~rootPath:(Some other));
  with_project "root-uri-precedence" (fun project ->
    let other = other_root project "other-root" in
    run_case
      project
      "rootUri over rootPath"
      ~workspaceFolders:None
      ~rootUri:(Uri.of_path project.root)
      ~rootPath:(Some other));
  with_project "root-path" (fun project ->
    run_case
      project
      "rootPath fallback"
      ~workspaceFolders:None
      ~rootPath:(Some project.root));
  with_project "cwd" (fun project ->
    run_case project "cwd fallback" ~workspaceFolders:None ~cwd:project.root);
  [%expect
    {|
    workspaceFolders over roots:
    {
      "uri": "<document-uri>",
      "diagnostics": [
        {
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ]
    }
    rootUri over rootPath:
    {
      "uri": "<document-uri>",
      "diagnostics": [
        {
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ]
    }
    rootPath fallback:
    {
      "uri": "<document-uri>",
      "diagnostics": [
        {
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ]
    }
    cwd fallback:
    {
      "uri": "<document-uri>",
      "diagnostics": [
        {
          "range": {
            "end": { "character": 0, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ]
    }
    |}]
;;
