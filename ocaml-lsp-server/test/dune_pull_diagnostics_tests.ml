open Lsp.Types
module Diagnostics = Ocaml_lsp_server.Diagnostics
module Pull = Ocaml_lsp_server.Dune_pull_diagnostics

let range =
  let start = Position.create ~line:0 ~character:0 in
  let end_ = Position.create ~line:0 ~character:1 in
  Range.create ~start ~end_
;;

let diagnostic ?source message =
  Diagnostic.create ?source ~range ~message:(`String message) ()
;;

let%expect_test "document diagnostics deduplicate Dune and Merlin" =
  let merlin = diagnostic ~source:Diagnostics.ocamllsp_source "duplicate" in
  let dune =
    [ diagnostic ~source:Diagnostics.dune_source "duplicate"
    ; diagnostic ~source:Diagnostics.dune_source "Dune only"
    ]
  in
  let merged = Diagnostics.merge ~merlin:[ merlin ] ~dune in
  let count_source source =
    List.filter
      (fun diagnostic ->
         Option.equal String.equal diagnostic.Diagnostic.source (Some source))
      merged
    |> List.length
  in
  Printf.printf
    "items=%d merlin=%d dune=%d\n"
    (List.length merged)
    (count_source Diagnostics.ocamllsp_source)
    (count_source Diagnostics.dune_source);
  [%expect {| items=2 merlin=1 dune=1 |}]
;;

let%expect_test "document diagnostics use a stable empty result" =
  let uri = Lsp.Uri.of_path "/workspace/clean.ml" in
  let params previousResultId =
    DocumentDiagnosticParams.create
      ~identifier:Pull.identifier
      ?previousResultId
      ~textDocument:(TextDocumentIdentifier.create ~uri)
      ()
  in
  let first = Pull.document [] (params None) in
  let result_id =
    match first with
    | `RelatedFullDocumentDiagnosticReport report ->
      Printf.printf "full items=%d\n" (List.length report.items);
      Option.get report.resultId
    | `RelatedUnchangedDocumentDiagnosticReport _ -> assert false
  in
  (match Pull.document [] (params (Some result_id)) with
   | `RelatedUnchangedDocumentDiagnosticReport _ -> print_endline "unchanged"
   | `RelatedFullDocumentDiagnosticReport _ -> assert false);
  [%expect
    {|
    full items=0
    unchanged |}]
;;
