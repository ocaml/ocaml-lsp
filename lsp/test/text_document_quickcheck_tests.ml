open Base
open Base_quickcheck
open Editing_model
module Text_document = Lsp.Text_document
module TextDocumentContentChangeEvent = Lsp.Types.TextDocumentContentChangeEvent

module TextDocumentContentChangeWholeDocument =
  Lsp.Types.TextDocumentContentChangeWholeDocument

module TextDocumentContentChangePartial = Lsp.Types.TextDocumentContentChangePartial
module TextDocumentItem = Lsp.Types.TextDocumentItem
module DidOpenTextDocumentParams = Lsp.Types.DidOpenTextDocumentParams
module DocumentUri = Lsp.Types.DocumentUri

type change =
  | Full_document of atom list
  | Ranged of int * int * atom list
[@@deriving quickcheck, sexp_of]

type version_update =
  | Keep_version
  | Set_change_version of int
[@@deriving quickcheck, sexp_of]

type operation =
  | Apply_changes of version_update * change list
  | Read_text
  | Query_position of int
  | Query_range of int * int
  | Set_version of int
[@@deriving quickcheck, sexp_of]

module Case = struct
  type t =
    { initial : atom list
    ; encoding : encoding
    ; operations : operation list
    }
  [@@deriving quickcheck, sexp_of]
end

type state =
  { text : string
  ; version : int
  ; encoding : encoding
  ; document : Text_document.t
  }

let normalized_version value = value land Stdlib.max_int

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

let fail state message =
  failf
    "%s\nmodel: text=%S version=%d\ndocument: text=%S version=%d"
    message
    state.text
    state.version
    (Text_document.text state.document)
    (Text_document.version state.document)
;;

let check_metadata state =
  if Text_document.version state.document <> state.version
  then fail state "document version differs";
  if
    not
      (Poly.equal
         (Text_document.position_encoding state.document)
         (lsp_encoding state.encoding))
  then fail state "position encoding differs"
;;

let check_text state =
  let actual = Text_document.text state.document in
  if not (String.equal state.text actual) then fail state "document text differs"
;;

let resolve_change text encoding = function
  | Full_document atoms ->
    let replacement = string_of_atoms atoms in
    ( replacement
    , `TextDocumentContentChangeWholeDocument
        (TextDocumentContentChangeWholeDocument.create ~text:replacement) )
  | Ranged (first_selector, second_selector, atoms) ->
    let first, second = ordered_cursors text encoding first_selector second_selector in
    let replacement = string_of_atoms atoms in
    let range = Range.create ~start:(position first) ~end_:(position second) in
    let text =
      replace text ~start:first.byte_offset ~stop:second.byte_offset replacement
    in
    ( text
    , `TextDocumentContentChangePartial
        (TextDocumentContentChangePartial.create ~range ~text:replacement ()) )
;;

let apply_changes state version_update changes =
  let text, changes =
    List.fold_map changes ~init:state.text ~f:(fun text change ->
      let text, change = resolve_change text state.encoding change in
      text, change)
  in
  let version =
    match version_update with
    | Keep_version -> None
    | Set_change_version version -> Some (normalized_version version)
  in
  let document = Text_document.apply_content_changes ?version state.document changes in
  let version = Option.value version ~default:state.version in
  { state with text; version; document }
;;

let apply_operation state = function
  | Apply_changes (version, changes) -> apply_changes state version changes
  | Read_text ->
    let first = Text_document.text state.document in
    let second = Text_document.text state.document in
    if not (String.equal state.text first) then fail state "document text differs";
    if not (String.equal first second) then fail state "repeated reads differ";
    state
  | Query_position selector ->
    let cursor = select_cursor state.text state.encoding selector in
    let actual = Text_document.absolute_position state.document (position cursor) in
    if actual <> cursor.byte_offset then fail state "absolute position differs";
    state
  | Query_range (first_selector, second_selector) ->
    let first, second =
      ordered_cursors state.text state.encoding first_selector second_selector
    in
    let range = Range.create ~start:(position first) ~end_:(position second) in
    let actual_start, actual_stop = Text_document.absolute_range state.document range in
    if actual_start <> first.byte_offset || actual_stop <> second.byte_offset
    then fail state "absolute range differs";
    state
  | Set_version version ->
    let version = normalized_version version in
    { state with version; document = Text_document.set_version state.document ~version }
;;

let run_case { Case.initial; encoding; operations } =
  let text = string_of_atoms initial in
  let state = { text; version = 0; encoding; document = make_document text encoding } in
  check_metadata state;
  check_text state;
  let final =
    List.fold_left operations ~init:state ~f:(fun state operation ->
      let state = apply_operation state operation in
      check_metadata state;
      state)
  in
  check_text final
;;

let regression_cases =
  [ { Case.initial = [ A; A; Newline; B; B ]
    ; encoding = UTF8
    ; operations =
        [ Apply_changes
            ( Set_change_version 1
            , [ Ranged (1, 4, [ Four_byte; Newline; A ]); Ranged (2, 3, [ B; B ]) ] )
        ; Query_position 4
        ; Query_range (0, 5)
        ; Read_text
        ]
    }
  ; { Case.initial = [ Four_byte; A; Newline; Three_byte ]
    ; encoding = UTF16
    ; operations =
        [ Apply_changes
            ( Keep_version
            , [ Full_document [ A; Four_byte; Newline; B ]
              ; Ranged (1, 3, [ Two_byte; Newline ])
              ] )
        ; Read_text
        ; Set_version 17
        ; Query_position 2
        ]
    }
  ; { Case.initial = []
    ; encoding = UTF8
    ; operations =
        [ Apply_changes
            ( Set_change_version 2
            , [ Ranged (0, 0, [ A ]); Ranged (1, 1, [ Newline; Four_byte ]) ] )
        ; Read_text
        ]
    }
  ]
;;

let%expect_test "content-change sequences agree with a string model" =
  Base_quickcheck.Test.run_exn (module Case) ~examples:regression_cases ~f:run_case;
  [%expect {| |}]
;;
