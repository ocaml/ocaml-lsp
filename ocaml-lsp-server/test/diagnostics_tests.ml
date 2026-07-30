open Lsp.Types
module Testing = Ocaml_lsp_server.Testing
module Diagnostics = Testing.Diagnostics

let run fiber = Lev_fiber.run (fun () -> fiber) |> Lev_fiber.Error.ok_exn

let range line =
  let start = Position.create ~line ~character:1 in
  let end_ = Position.create ~line ~character:4 in
  Range.create ~start ~end_
;;

let diagnostic ?(source = Diagnostics.ocamllsp_source) ?tags ~range message =
  Diagnostic.create ~range ~source ~message:(`String message) ?tags ()
;;

let print_publications publications =
  List.iter
    (fun publication ->
       PublishDiagnosticsParams.yojson_of_t publication
       |> Yojson.Safe.pretty_to_string ~std:false
       |> print_endline)
    (List.concat (List.rev !publications))
;;

let%expect_test "merge and filter Merlin and Dune diagnostics" =
  let publications = ref [] in
  let capabilities =
    let tagSupport =
      ClientDiagnosticsTagOptions.create
        ~valueSet:[ DiagnosticTag.Unnecessary; DiagnosticTag.Deprecated ]
    in
    PublishDiagnosticsClientCapabilities.create ~relatedInformation:true ~tagSupport ()
  in
  let diagnostics =
    Diagnostics.create
      (Some capabilities)
      (fun params ->
         publications := params :: !publications;
         Fiber.return ())
      ~report_dune_diagnostics:true
      ~shorten_merlin_diagnostics:false
  in
  let uri = DocumentUri.of_path "/workspace/main.ml" in
  let same_range = range 2 in
  Diagnostics.set
    diagnostics
    (`Merlin
        ( uri
        , [ diagnostic ~range:same_range "Type mismatch: int but unit"
          ; diagnostic ~range:(range 8) "Merlin-only"
          ] ));
  let dune = Diagnostics.Dune.gen (Testing.Pid.of_int 10) in
  Diagnostics.set
    diagnostics
    (`Dune
        ( dune
        , Dune_rpc.V1.Diagnostic.Id.create 1
        , uri
        , diagnostic
            ~source:Diagnostics.dune_source
            ~range:same_range
            "Type mismatch:   int\nbut unit" ));
  run (Diagnostics.send diagnostics `All);
  print_endline "deduplicated:";
  print_publications publications;
  publications := [];
  Diagnostics.set
    diagnostics
    (`Dune
        ( dune
        , Dune_rpc.V1.Diagnostic.Id.create 2
        , uri
        , diagnostic ~source:Diagnostics.dune_source ~range:same_range "Different" ));
  run (Diagnostics.send diagnostics `All);
  print_endline "different message:";
  print_publications publications;
  publications := [];
  run (Diagnostics.set_report_dune_diagnostics diagnostics ~report_dune_diagnostics:false);
  print_endline "Dune disabled:";
  print_publications publications;
  let print_tag label src message =
    let tags = Diagnostics.tags_of_message diagnostics ~src message in
    Printf.printf
      "%s: %s\n"
      label
      (match tags with
       | None -> "none"
       | Some tags ->
         `List (List.map DiagnosticTag.yojson_of_t tags) |> Yojson.Safe.to_string)
  in
  print_tag "Dune unused" `Dune "unused library";
  print_tag "Merlin deprecated" `Merlin "Error (alert deprecated): use new_value";
  print_tag "ordinary" `Merlin "Unbound value x";
  [%expect
    {|
    deduplicated:
    {
      "diagnostics": [
        {
          "message": "Type mismatch: int but unit",
          "range": {
            "end": { "character": 4, "line": 2 },
            "start": { "character": 1, "line": 2 }
          },
          "source": "ocamllsp"
        },
        {
          "message": "Merlin-only",
          "range": {
            "end": { "character": 4, "line": 8 },
            "start": { "character": 1, "line": 8 }
          },
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///workspace/main.ml"
    }
    different message:
    {
      "diagnostics": [
        {
          "message": "Different",
          "range": {
            "end": { "character": 4, "line": 2 },
            "start": { "character": 1, "line": 2 }
          },
          "source": "dune"
        },
        {
          "message": "Type mismatch: int but unit",
          "range": {
            "end": { "character": 4, "line": 2 },
            "start": { "character": 1, "line": 2 }
          },
          "source": "ocamllsp"
        },
        {
          "message": "Merlin-only",
          "range": {
            "end": { "character": 4, "line": 8 },
            "start": { "character": 1, "line": 8 }
          },
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///workspace/main.ml"
    }
    Dune disabled:
    {
      "diagnostics": [
        {
          "message": "Type mismatch: int but unit",
          "range": {
            "end": { "character": 4, "line": 2 },
            "start": { "character": 1, "line": 2 }
          },
          "source": "ocamllsp"
        },
        {
          "message": "Merlin-only",
          "range": {
            "end": { "character": 4, "line": 8 },
            "start": { "character": 1, "line": 8 }
          },
          "source": "ocamllsp"
        }
      ],
      "uri": "file:///workspace/main.ml"
    }
    Dune unused: [1]
    Merlin deprecated: [2]
    ordinary: none
    |}]
;;
