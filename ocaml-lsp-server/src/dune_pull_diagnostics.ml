open Import

let identifier = "ocamllsp"
let diagnostic_json diagnostic = Diagnostic.yojson_of_t diagnostic |> Json.to_string

let sort_diagnostics diagnostics =
  List.map diagnostics ~f:(fun diagnostic -> diagnostic_json diagnostic, diagnostic)
  |> List.sort ~compare:(fun (x, _) (y, _) -> Poly.compare x y)
  |> List.map ~f:snd
;;

let result_id diagnostics =
  let json = `List (List.map diagnostics ~f:Diagnostic.yojson_of_t) in
  let digest = Json.to_string json |> Stdlib.Digest.string |> Stdlib.Digest.to_hex in
  identifier ^ ":" ^ digest
;;

let document diagnostics (params : DocumentDiagnosticParams.t) =
  let items = sort_diagnostics diagnostics in
  let result_id = result_id items in
  match params.previousResultId with
  | Some previous when String.equal previous result_id ->
    `RelatedUnchangedDocumentDiagnosticReport
      (RelatedUnchangedDocumentDiagnosticReport.create ~resultId:result_id ())
  | None | Some _ ->
    `RelatedFullDocumentDiagnosticReport
      (RelatedFullDocumentDiagnosticReport.create ~items ~resultId:result_id ())
;;
