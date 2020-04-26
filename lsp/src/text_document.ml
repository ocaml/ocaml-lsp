open Types

type t = TextDocumentItem.t

let make (t : DidOpenTextDocumentParams.t) =
  let text = Text_document_text.normalize_line_endings t.textDocument.text in
  { t.textDocument with text }

let documentUri (doc : t) = Uri.t_of_yojson (`String doc.uri)

let version (t : t) = t.version

let text (t : t) = t.text

let apply_content_change ?version (change : TextDocumentContentChangeEvent.t)
    (doc : t) =
  let version =
    match version with
    | None -> doc.version + 1
    | Some version -> version
  in
  let text =
    match change.range with
    | None -> Text_document_text.normalize_line_endings change.text
    | Some range -> Text_document_text.apply_change doc.text range change.text
  in
  { doc with version; text }

let languageId (t : t) = t.languageId
