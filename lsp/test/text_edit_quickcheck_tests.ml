open Base
open Base_quickcheck
open Editing_model
module Text_document = Lsp.Text_document
module TextEdit = Lsp.Types.TextEdit
module TextDocumentItem = Lsp.Types.TextDocumentItem
module DidOpenTextDocumentParams = Lsp.Types.DidOpenTextDocumentParams
module DocumentUri = Lsp.Types.DocumentUri

type raw_edit =
  { first : int
  ; second : int
  ; replacement : atom list
  }
[@@deriving quickcheck, sexp_of]

module Edit_case = struct
  type t =
    { initial : atom list
    ; encoding : encoding
    ; edits : raw_edit list
    }
  [@@deriving quickcheck, sexp_of]
end

module Diff_case = struct
  type t =
    { from : atom list
    ; to_ : atom list
    ; encoding : encoding
    }
  [@@deriving quickcheck, sexp_of]
end

type edit =
  { index : int
  ; start : cursor
  ; stop : cursor
  ; replacement : string
  }

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

let compare_edit left right =
  match Int.compare left.start.byte_offset right.start.byte_offset with
  | 0 -> Int.compare left.index right.index
  | ordering -> ordering
;;

let normalize_edits text encoding edits =
  let candidates =
    List.mapi edits ~f:(fun index { first; second; replacement } ->
      let start, stop = ordered_cursors text encoding first second in
      { index; start; stop; replacement = string_of_atoms replacement })
    |> List.sort ~compare:compare_edit
  in
  let _, accepted =
    List.fold candidates ~init:(None, []) ~f:(fun (previous, accepted) candidate ->
      let accept =
        match previous with
        | None -> true
        | Some previous ->
          let starts_after_previous =
            candidate.start.byte_offset > previous.start.byte_offset
            && candidate.start.byte_offset >= previous.stop.byte_offset
          in
          let insertions_at_same_position =
            candidate.start.byte_offset = previous.start.byte_offset
            && candidate.start.byte_offset = candidate.stop.byte_offset
            && previous.start.byte_offset = previous.stop.byte_offset
          in
          starts_after_previous || insertions_at_same_position
      in
      if accept then Some candidate, candidate :: accepted else previous, accepted)
  in
  List.rev accepted
;;

let text_edit edit =
  let range = Range.create ~start:(position edit.start) ~end_:(position edit.stop) in
  TextEdit.create ~range ~newText:edit.replacement
;;

let apply_model text edits =
  let buffer = Buffer.create (String.length text) in
  let offset = ref 0 in
  List.iter edits ~f:(fun edit ->
    Buffer.add_substring buffer text ~pos:!offset ~len:(edit.start.byte_offset - !offset);
    Buffer.add_string buffer edit.replacement;
    offset := edit.stop.byte_offset);
  Buffer.add_substring buffer text ~pos:!offset ~len:(String.length text - !offset);
  Buffer.contents buffer
;;

let apply_document text encoding edits =
  let document = make_document text encoding in
  Text_document.apply_text_document_edits document (List.map edits ~f:text_edit)
  |> Text_document.text
;;

let duplicate_starts edits =
  let rec loop = function
    | [] | [ _ ] -> false
    | first :: (second :: _ as rest) ->
      first.start.byte_offset = second.start.byte_offset || loop rest
  in
  loop edits
;;

let run_edit_case { Edit_case.initial; encoding; edits } =
  let text = string_of_atoms initial in
  let edits = normalize_edits text encoding edits in
  let expected = apply_model text edits in
  let actual = apply_document text encoding edits in
  if not (String.equal expected actual)
  then
    failf "text edits differ\nsource: %S\nexpected: %S\nactual: %S" text expected actual;
  if not (duplicate_starts edits)
  then (
    let reversed = apply_document text encoding (List.rev edits) in
    if not (String.equal expected reversed)
    then
      failf
        "distinct text edits depend on input order\nsource: %S\nexpected: %S\nactual: %S"
        text
        expected
        reversed)
;;

let run_diff_case { Diff_case.from; to_; encoding } =
  let from = string_of_atoms from in
  let to_ = string_of_atoms to_ in
  let edits = Lsp.Diff.edit ~from ~to_ in
  let document = make_document from encoding in
  let actual =
    Text_document.apply_text_document_edits document edits |> Text_document.text
  in
  if not (String.equal to_ actual)
  then failf "diff round trip failed\nfrom: %S\nto: %S\nactual: %S" from to_ actual
;;

let edit_examples =
  [ { Edit_case.initial = [ A; A; Newline; B; B ]
    ; encoding = UTF8
    ; edits =
        [ { first = 1; second = 1; replacement = [ Four_byte; Newline ] }
        ; { first = 4; second = 5; replacement = [] }
        ]
    }
  ; { Edit_case.initial = [ Four_byte; A; Newline; Three_byte; B ]
    ; encoding = UTF16
    ; edits =
        [ { first = 1; second = 2; replacement = [ Two_byte ] }
        ; { first = 4; second = 4; replacement = [ A ] }
        ]
    }
  ; { Edit_case.initial = [ A; B ]
    ; encoding = UTF8
    ; edits =
        [ { first = 1; second = 1; replacement = [ A ] }
        ; { first = 1; second = 1; replacement = [ B ] }
        ]
    }
  ]
;;

let diff_examples =
  [ { Diff_case.from = []; to_ = [ Four_byte; Newline ]; encoding = UTF8 }
  ; { Diff_case.from = [ A; Newline; Four_byte ]
    ; to_ = [ Three_byte; Newline; B; Newline ]
    ; encoding = UTF16
    }
  ]
;;

let%expect_test "simultaneous text edits agree with a string model" =
  Base_quickcheck.Test.run_exn (module Edit_case) ~examples:edit_examples ~f:run_edit_case;
  [%expect {| |}]
;;

let%expect_test "diff edits reconstruct the target text" =
  Base_quickcheck.Test.run_exn (module Diff_case) ~examples:diff_examples ~f:run_diff_case;
  [%expect {| |}]
;;
