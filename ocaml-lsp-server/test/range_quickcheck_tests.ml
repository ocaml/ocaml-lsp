open Base
open Base_quickcheck
open Editing_model
module Server_range = Ocaml_lsp_server.Testing.Range
module Text_document = Lsp.Text_document
module TextEdit = Lsp.Types.TextEdit
module TextDocumentItem = Lsp.Types.TextDocumentItem
module DidOpenTextDocumentParams = Lsp.Types.DidOpenTextDocumentParams
module DocumentUri = Lsp.Types.DocumentUri

module Case = struct
  type t =
    { prefix : atom list
    ; removed : atom list
    ; replacement : atom list
    ; suffix : atom list
    ; encoding : encoding
    }
  [@@deriving quickcheck, sexp_of]
end

let make_document text encoding =
  let textDocument =
    TextDocumentItem.create
      ~uri:(DocumentUri.of_path "/quickcheck.ml")
      ~languageId:(Lsp.Types.LanguageKind.Other "ocaml")
      ~version:0
      ~text
  in
  Text_document.make
    ~position_encoding:(lsp_encoding encoding)
    (DidOpenTextDocumentParams.create ~textDocument)
;;

let equal_position (left : Position.t) (right : Position.t) =
  left.line = right.line && left.character = right.character
;;

let show_position (position : Position.t) =
  Printf.sprintf "%d:%d" position.line position.character
;;

let run_case { Case.prefix; removed; replacement; suffix; encoding } =
  let prefix = string_of_atoms prefix in
  let removed = string_of_atoms removed in
  let replacement = string_of_atoms replacement in
  let suffix = string_of_atoms suffix in
  let source = prefix ^ removed ^ suffix in
  let start = cursor_at_byte_offset source encoding (String.length prefix) in
  let stop =
    cursor_at_byte_offset source encoding (String.length prefix + String.length removed)
  in
  let edit =
    TextEdit.create
      ~range:(Range.create ~start:(position start) ~end_:(position stop))
      ~newText:replacement
  in
  let resized =
    Server_range.resize_for_edit ~position_encoding:(lsp_encoding encoding) edit
  in
  let expected_end = position_after_text (position start) replacement encoding in
  if not (equal_position resized.start (position start))
  then failf "resized range changed its start";
  if not (equal_position resized.end_ expected_end)
  then
    failf
      "resized range has the wrong end\n\
       start: %s\n\
       replacement: %S\n\
       expected: %s\n\
       actual: %s"
      (show_position (position start))
      replacement
      (show_position expected_end)
      (show_position resized.end_);
  let document = make_document source encoding in
  let document = Text_document.apply_text_document_edits document [ edit ] in
  let actual_text = Text_document.text document in
  let expected_text = prefix ^ replacement ^ suffix in
  if not (String.equal expected_text actual_text)
  then
    failf "edit application differs\nexpected: %S\nactual: %S" expected_text actual_text;
  let range_start, range_stop = Text_document.absolute_range document resized in
  let selected = slice actual_text ~start:range_start ~stop:range_stop in
  if not (String.equal replacement selected)
  then
    failf
      "resized range does not select the inserted text\n\
       text: %S\n\
       replacement: %S\n\
       selected: %S"
      actual_text
      replacement
      selected
;;

let regression_cases =
  [ { Case.prefix = [ A; A ]
    ; removed = []
    ; replacement = [ A; A; Newline; B; B; B ]
    ; suffix = [ Space; A ]
    ; encoding = UTF8
    }
  ; { Case.prefix = [ Four_byte; A ]
    ; removed = [ B; Newline ]
    ; replacement = []
    ; suffix = [ Three_byte ]
    ; encoding = UTF16
    }
  ; { Case.prefix = [ A; Newline; B ]
    ; removed = [ B ]
    ; replacement = [ Two_byte; Newline; Four_byte ]
    ; suffix = [ A; B ]
    ; encoding = UTF16
    }
  ]
;;

let%expect_test "resized edit ranges select exactly the replacement text" =
  Base_quickcheck.Test.run_exn (module Case) ~examples:regression_cases ~f:run_case;
  [%expect {| |}]
;;
