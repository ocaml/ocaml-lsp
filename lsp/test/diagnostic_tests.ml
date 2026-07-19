open Lsp.Types

let print json = Yojson.Safe.pretty_to_string json |> print_endline

let%expect_test "workspace diagnostic reports with absent versions" =
  let uri = Lsp.Uri.of_path "/workspace/test.ml" in
  WorkspaceFullDocumentDiagnosticReport.create ~items:[] ~resultId:"full-result" ~uri ()
  |> WorkspaceFullDocumentDiagnosticReport.yojson_of_t
  |> print;
  WorkspaceUnchangedDocumentDiagnosticReport.create ~resultId:"unchanged-result" ~uri ()
  |> WorkspaceUnchangedDocumentDiagnosticReport.yojson_of_t
  |> print;
  [%expect
    {|
    {
      "kind": "full",
      "items": [],
      "resultId": "full-result",
      "uri": "file:///workspace/test.ml",
      "version": null
    }
    {
      "kind": "unchanged",
      "resultId": "unchanged-result",
      "uri": "file:///workspace/test.ml",
      "version": null
    }
    |}]
;;
