open Base
open Base_quickcheck
open Editing_model
module Compl = Ocaml_lsp_server.Testing.Compl

module Ident_atom = struct
  type t =
    | A
    | B
    | Two_byte
    | Three_byte
    | Four_byte
  [@@deriving quickcheck, sexp_of]

  let to_string = function
    | A -> "a"
    | B -> "b"
    | Two_byte -> "é"
    | Three_byte -> "€"
    | Four_byte -> "😀"
  ;;
end

let string_of_ident atoms = List.map atoms ~f:Ident_atom.to_string |> String.concat

module Case = struct
  type t =
    { before : atom list
    ; prefix : Ident_atom.t list
    ; suffix : Ident_atom.t list
    ; encoding : encoding
    }
  [@@deriving quickcheck, sexp_of]
end

let equal_position (left : Lsp.Types.Position.t) (right : Lsp.Types.Position.t) =
  left.line = right.line && left.character = right.character
;;

let equal_range (left : Lsp.Types.Range.t) (right : Lsp.Types.Range.t) =
  equal_position left.start right.start && equal_position left.end_ right.end_
;;

let check label condition = if not condition then failwith label

let check_case { Case.before; prefix; suffix; encoding } =
  let before = string_of_atoms before in
  let prefix = string_of_ident prefix in
  let suffix = string_of_ident suffix in
  let text = before ^ prefix ^ suffix in
  let prefix_start = cursor_at_byte_offset text encoding (String.length before) in
  let cursor =
    cursor_at_byte_offset text encoding (String.length before + String.length prefix)
  in
  let suffix_end = cursor_at_byte_offset text encoding (String.length text) in
  let prefix_range =
    Compl.For_tests.range_prefix
      ~position_encoding:(lsp_encoding encoding)
      (position cursor)
      prefix
  in
  let expected_prefix_range =
    Lsp.Types.Range.create ~start:(position prefix_start) ~end_:(position cursor)
  in
  check "completion prefix range" (equal_range prefix_range expected_prefix_range);
  let identifier_range =
    Compl.For_tests.identifier_range
      ~position_encoding:(lsp_encoding encoding)
      (position cursor)
      ~prefix
      ~suffix
  in
  let expected_identifier_range =
    Lsp.Types.Range.create ~start:(position prefix_start) ~end_:(position suffix_end)
  in
  check
    "completion identifier range"
    (equal_range identifier_range expected_identifier_range)
;;

let regression_cases =
  [ { Case.before = [ Four_byte; Newline; A ]
    ; prefix = [ Ident_atom.Four_byte; Two_byte ]
    ; suffix = [ Three_byte; B ]
    ; encoding = UTF16
    }
  ; { before = [ Three_byte ]; prefix = [ Two_byte ]; suffix = []; encoding = UTF8 }
  ; { before = []; prefix = []; suffix = [ Four_byte ]; encoding = UTF16 }
  ]
;;

let%test_unit "completion ranges select prefixes and identifiers" =
  Test.run_exn (module Case) ~examples:regression_cases ~f:check_case
;;
