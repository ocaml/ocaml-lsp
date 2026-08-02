open Test.Import
open Dune_rpc_test

type project =
  { temp : string
  ; root : string
  ; runtime_dir : string
  ; ml : string
  ; mli : string
  ; gate : string
  ; dune_pid : int
  }

let create_project () =
  let temp = Test.temp_dir "ocamllsp-ml-mli-mismatch-" in
  let root = Filename.concat temp "workspace" in
  let runtime_dir = Filename.concat temp "runtime" in
  Unix.mkdir root 0o700;
  Unix.mkdir runtime_dir 0o700;
  Test.write_file (Filename.concat root "dune-project") "(lang dune 3.24)\n";
  let ml = Filename.concat root "foo.ml" in
  let mli = Filename.concat root "foo.mli" in
  let gate = Filename.concat temp "gate" in
  Test.write_file mli "val x : unit\n";
  Test.write_file ml "let x = 123\n";
  let wait_script = Filename.concat root "wait.sh" in
  Test.write_file wait_script "#!/bin/sh\nwhile [ ! -e \"$1\" ]; do sleep 0.01; done\n";
  Unix.chmod wait_script 0o755;
  (* Keep the Dune build blocked until ocamllsp is connected, then compile the
     mismatched ml/mli pair so Dune RPC emits the diagnostic. *)
  Test.write_file
    (Filename.concat root "dune")
    (Printf.sprintf
       {jbuild|
(rule
 (alias repro)
 (deps wait.sh foo.ml foo.mli)
 (action
  (progn
   (run %%{dep:wait.sh} %s)
   (run ocamlc -c foo.mli)
   (run ocamlc -c foo.ml))))
|jbuild}
       gate);
  let dune_pid = start_dune root runtime_dir in
  { temp; root; runtime_dir; ml; mli; gate; dune_pid }
;;

let destroy_project project =
  stop_process project.dune_pid;
  ignore (Sys.command ("rm -rf -- " ^ Filename.quote project.temp) : int)
;;

let sanitize_string project string =
  (* Replace mli paths before ml paths so foo.mli is not partially rewritten. *)
  let replacements =
    [ Uri.to_string (Uri.of_path project.mli), "<mli-uri>"
    ; Uri.to_string (Uri.of_path project.ml), "<ml-uri>"
    ; project.mli, "<mli-path>"
    ; project.ml, "<ml-path>"
    ; project.root, "<workspace>"
    ; project.temp, "<test-dir>"
    ]
  in
  List.fold_left replacements ~init:string ~f:(fun string (pattern, with_) ->
    String.substr_replace_all string ~pattern ~with_)
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
  only_dune_diagnostics params
  |> PublishDiagnosticsParams.yojson_of_t
  |> sanitize_json project
  |> Test.print_result
;;

let capabilities =
  let publishDiagnostics =
    PublishDiagnosticsClientCapabilities.create ~relatedInformation:true ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~publishDiagnostics () in
  let window = WindowClientCapabilities.create ~workDoneProgress:true () in
  ClientCapabilities.create ~textDocument ~window ()
;;

let%expect_test "dune reports related diagnostics for mismatched ml/mli files" =
  let project = create_project () in
  Fun.protect
    ~finally:(fun () -> destroy_project project)
    (fun () ->
       wait_for_rpc_registration project.runtime_dir project.dune_pid;
       let events = Lifecycle_events.create () in
       run_with_workspace
         ~capabilities
         ~root:project.root
         ~runtime_dir:project.runtime_dir
         events
         ~f:(fun _client _workspace ->
           let* () = Signal.wait (Events.dune_ready events.dune) in
           Test.write_file project.gate "";
           let* params =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri (Uri.of_path project.ml) params && has_dune_diagnostic params)
           in
           print_publication project "Dune ml/mli mismatch:" params;
           Fiber.return ()));
  [%expect
    {|
    Dune ml/mli mismatch:
    {
      "diagnostics": [
        {
          "message": "The implementation foo.ml does not match the interface foo.mli: \nValues do not match: val x : int is not included in val x : unit\nThe type int is not compatible with the type unit",
          "range": {
            "end": { "character": 5, "line": 0 },
            "start": { "character": 4, "line": 0 }
          },
          "relatedInformation": [
            {
              "location": {
                "range": {
                  "end": { "character": 12, "line": 0 },
                  "start": { "character": 0, "line": 0 }
                },
                "uri": "<mli-uri>"
              },
              "message": "Expected declaration"
            },
            {
              "location": {
                "range": {
                  "end": { "character": 5, "line": 0 },
                  "start": { "character": 4, "line": 0 }
                },
                "uri": "<ml-uri>"
              },
              "message": "Actual declaration"
            }
          ],
          "severity": 1,
          "source": "dune"
        }
      ],
      "uri": "<ml-uri>"
    }
    |}]
;;
