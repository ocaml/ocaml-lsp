open Test.Import
open Dune_rpc_test
module D = Dune_rpc.Private.Diagnostic
module Fake = Dune_rpc_fake

let capabilities =
  let publishDiagnostics = PublishDiagnosticsClientCapabilities.create () in
  let textDocument = TextDocumentClientCapabilities.create ~publishDiagnostics () in
  let window = WindowClientCapabilities.create ~workDoneProgress:true () in
  ClientCapabilities.create ~textDocument ~window ()
;;

let position ~fname ~line ~character =
  let open Lexing in
  { pos_fname = fname; pos_lnum = line; pos_bol = 0; pos_cnum = character }
;;

let fake_diagnostic ~fname ~diagnostic_id ~diagnostic_message ~line ~start_char ~end_char =
  let open D in
  let diagnostic_loc =
    let start = position ~fname ~line ~character:start_char in
    let stop = position ~fname ~line ~character:end_char in
    Stdune.Lexbuf.Loc.{ start; stop }
  in
  { targets = []
  ; id = Id.create diagnostic_id
  ; message = Stdune.Pp.text diagnostic_message
  ; loc = Some diagnostic_loc
  ; severity = Some Error
  ; promotion = []
  ; directory = None
  ; related = []
  }
;;

let source_uri fake = Uri.of_path (Filename.concat (Fake.root fake) "main.ml")

let print_publication fake label params =
  print_endline label;
  PublishDiagnosticsParams.yojson_of_t params
  |> fun json ->
  (* The workspace root is a randomized temp directory; keep it out of the
     expected output. *)
  let root_uri = Uri.to_string (Uri.of_path (Fake.root fake)) in
  let json =
    match json with
    | `Assoc fields ->
      `Assoc
        (List.map fields ~f:(fun (name, value) ->
           let rec sanitize = function
             | `String s ->
               `String (String.substr_replace_all s ~pattern:root_uri ~with_:"<root>")
             | `Assoc fields -> `Assoc (List.map fields ~f:(fun (n, v) -> n, sanitize v))
             | `List values -> `List (List.map values ~f:sanitize)
             | json -> json
           in
           name, sanitize value))
    | json -> json
  in
  Test.print_result json
;;

let%expect_test
    "keep Dune and Merlin diagnostics with different messages at the same range"
  =
  let fake =
    Fake.start "different" ~diagnostics:(fun root ->
      let source = Filename.concat root "main.ml" in
      [ [ D.Event.Add
            (fake_diagnostic
               ~fname:source
               ~diagnostic_id:1
               ~diagnostic_message:"dune says wrong"
               ~line:1
               ~start_char:21
               ~end_char:22)
        ]
      ])
  in
  Fun.protect
    ~finally:(fun () -> Fake.stop fake)
    (fun () ->
       let events = Lifecycle_events.create () in
       run_with_workspace
         ~capabilities
         ~root:(Fake.root fake)
         ~runtime_dir:(Fake.runtime_dir fake)
         events
         ~f:(fun client _workspace ->
           let uri = source_uri fake in
           let* () = open_document client ~uri ~text:"let value : string = 1\n" in
           let+ merged =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri uri params
               && List.exists params.diagnostics ~f:(fun (d : Diagnostic.t) ->
                 Option.equal String.equal d.source (Some "dune"))
               && List.exists params.diagnostics ~f:(fun (d : Diagnostic.t) ->
                 Option.equal String.equal d.source (Some "ocamllsp")))
           in
           print_publication fake "merged:" merged));
  [%expect
    {|
    merged:
    {
      "diagnostics": [
        {
          "message": "No config found for file main.ml. Try calling 'dune build'.",
          "range": {
            "end": { "character": 0, "line": 1 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "ocamllsp"
        },
        {
          "message": "dune says\nwrong",
          "range": {
            "end": { "character": 22, "line": 0 },
            "start": { "character": 21, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        },
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
      "uri": "<root>/main.ml"
    }
    |}]
;;

let%expect_test "removes a Dune diagnostic published earlier" =
  let fake =
    Fake.start "removal" ~diagnostics:(fun root ->
      let source = Filename.concat root "main.ml" in
      [ [ D.Event.Add
            (fake_diagnostic
               ~fname:source
               ~diagnostic_id:1
               ~diagnostic_message:"transient dune error"
               ~line:1
               ~start_char:0
               ~end_char:3)
        ]
      ; [ D.Event.Remove
            (fake_diagnostic
               ~fname:source
               ~diagnostic_id:1
               ~diagnostic_message:""
               ~line:1
               ~start_char:0
               ~end_char:3)
        ]
      ])
  in
  Fun.protect
    ~finally:(fun () -> Fake.stop fake)
    (fun () ->
       let events = Lifecycle_events.create () in
       run_with_workspace
         ~capabilities
         ~root:(Fake.root fake)
         ~runtime_dir:(Fake.runtime_dir fake)
         events
         ~f:(fun client _workspace ->
           let uri = source_uri fake in
           let* () = open_document client ~uri ~text:"let value = 1\n" in
           let* published =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri uri params && has_dune_diagnostic params)
           in
           print_publication fake "with dune diagnostic:" published;
           let+ cleared =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri uri params && no_dune_diagnostic params)
           in
           print_publication fake "after removal:" cleared));
  [%expect
    {|
    with dune diagnostic:
    {
      "diagnostics": [
        {
          "message": "transient dune\nerror",
          "range": {
            "end": { "character": 3, "line": 0 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ],
      "uri": "<root>/main.ml"
    }
    after removal:
    { "diagnostics": [], "uri": "<root>/main.ml" }
    |}]
;;

let%expect_test "Dune diagnostic without a location uses the workspace root" =
  let fake =
    Fake.start "no-loc" ~diagnostics:(fun _root ->
      [ [ D.Event.Add
            { (fake_diagnostic
                 ~fname:"unused"
                 ~diagnostic_id:1
                 ~diagnostic_message:"build failed"
                 ~line:1
                 ~start_char:0
                 ~end_char:1)
              with
              loc = None
            }
        ]
      ])
  in
  Fun.protect
    ~finally:(fun () -> Fake.stop fake)
    (fun () ->
       let events = Lifecycle_events.create () in
       run_with_workspace
         ~capabilities
         ~root:(Fake.root fake)
         ~runtime_dir:(Fake.runtime_dir fake)
         events
         ~f:(fun client _workspace ->
           let uri = source_uri fake in
           let* () = open_document client ~uri ~text:"let value = 1\n" in
           let+ published =
             Events.wait_for_diagnostics events.dune ~f:(fun params ->
               for_uri (Uri.of_path (Fake.root fake)) params && has_dune_diagnostic params)
           in
           print_publication fake "root diagnostic:" published));
  [%expect
    {|
    root diagnostic:
    {
      "diagnostics": [
        {
          "message": "build\nfailed",
          "range": {
            "end": { "character": 0, "line": 1 },
            "start": { "character": 0, "line": 0 }
          },
          "severity": 1,
          "source": "dune"
        }
      ],
      "uri": "<root>"
    }
    |}]
;;

let%expect_test "removing an unknown promotion is a no-op" =
  let fake =
    Fake.start "unknown-promotion" ~diagnostics:(fun root ->
      let source = Filename.concat root "main.ml" in
      let promotion =
        D.Promotion.
          { in_build = Filename.concat root "_build/default/main.ml"; in_source = source }
      in
      [ [ D.Event.Remove
            { (fake_diagnostic
                 ~fname:source
                 ~diagnostic_id:1
                 ~diagnostic_message:""
                 ~line:1
                 ~start_char:0
                 ~end_char:1)
              with
              promotion = [ promotion ]
            }
        ]
      ])
  in
  Fun.protect
    ~finally:(fun () -> Fake.stop fake)
    (fun () ->
       let events = Lifecycle_events.create () in
       run_with_workspace
         ~capabilities
         ~root:(Fake.root fake)
         ~runtime_dir:(Fake.runtime_dir fake)
         events
         ~f:(fun client _workspace ->
           let uri = source_uri fake in
           let* () = open_document client ~uri ~text:"let value = 1\n" in
           let* () = Lev_fiber.Timer.sleepf 0.5 in
           print_endline "no crash after unknown promotion removal";
           let* echo = Client.request client (DebugEcho { message = "still alive" }) in
           Printf.printf "echo: %s\n" echo.message;
           Fiber.return ()));
  [%expect
    {|
    no crash after unknown promotion removal
    echo: still alive
    |}]
;;
