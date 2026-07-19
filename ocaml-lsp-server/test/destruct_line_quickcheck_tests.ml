open Base
open Base_quickcheck
open Editing_model
module Destruct_line = Ocaml_lsp_server.Testing.Destruct_line

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

type statement_kind =
  | Match
  | Match_with
  | Case
[@@deriving quickcheck, sexp_of]

module Case = struct
  type t =
    { kind : statement_kind
    ; indent_selector : int
    ; trailing_selector : int
    ; expression : Ident_atom.t list
    ; encoding : encoding
    }
  [@@deriving quickcheck, sexp_of]
end

let select selector ~choices = Int.rem (selector land Stdlib.max_int) choices
let spaces selector = String.make (select selector ~choices:8) ' '

let expression atoms =
  match List.map atoms ~f:Ident_atom.to_string |> String.concat with
  | "" -> "x"
  | expression -> expression
;;

let equal_position (left : Lsp.Types.Position.t) (right : Lsp.Types.Position.t) =
  left.line = right.line && left.character = right.character
;;

let equal_range (left : Lsp.Types.Range.t) (right : Lsp.Types.Range.t) =
  equal_position left.start right.start && equal_position left.end_ right.end_
;;

let check label condition = if not condition then failwith label

let range_of_offsets text encoding start stop =
  let start = cursor_at_byte_offset text encoding start |> position in
  let end_ = cursor_at_byte_offset text encoding stop |> position in
  Lsp.Types.Range.create ~start ~end_
;;

let check_case
      { Case.kind; indent_selector; trailing_selector; expression = atoms; encoding }
  =
  let indent = spaces indent_selector in
  let trailing = spaces trailing_selector in
  let expression = expression atoms in
  let before_expression, after_expression =
    match kind with
    | Match -> indent ^ "match ", ""
    | Match_with -> indent ^ "match ", " with"
    | Case -> indent ^ "| ", " -> rhs"
  in
  let logical_line = before_expression ^ expression ^ after_expression in
  let code = logical_line ^ trailing ^ "\n" in
  let expression_start = String.length before_expression in
  let expression_end = expression_start + String.length expression in
  let cursor = cursor_at_byte_offset code encoding expression_end |> position in
  let input_range = Lsp.Types.Range.create ~start:cursor ~end_:cursor in
  let query_range, reply_range =
    Destruct_line.ranges
      ~position_encoding:(lsp_encoding encoding)
      ~code
      ~range:input_range
    |> Option.value_exn
  in
  let expected_query = range_of_offsets code encoding expression_start expression_end in
  check "destruct query range" (equal_range query_range expected_query);
  let expected_reply =
    match kind with
    | Case -> expected_query
    | Match | Match_with ->
      range_of_offsets code encoding (String.length indent) (String.length logical_line)
  in
  check "destruct reply range" (equal_range reply_range expected_reply)
;;

let regression_cases =
  [ { Case.kind = Match_with
    ; indent_selector = 2
    ; trailing_selector = 3
    ; expression = [ Ident_atom.Four_byte; Two_byte ]
    ; encoding = UTF16
    }
  ; { kind = Case
    ; indent_selector = 1
    ; trailing_selector = 0
    ; expression = [ Three_byte; Ident_atom.A ]
    ; encoding = UTF8
    }
  ]
;;

let check_hole_ranges encoding =
  let check_one ~code ~cursor_offset ~expected_offset =
    let cursor = cursor_at_byte_offset code encoding cursor_offset |> position in
    let range = Lsp.Types.Range.create ~start:cursor ~end_:cursor in
    let query_range, reply_range =
      Destruct_line.ranges ~position_encoding:(lsp_encoding encoding) ~code ~range
      |> Option.value_exn
    in
    let expected = range_of_offsets code encoding expected_offset expected_offset in
    check "destruct hole query range" (equal_range query_range expected);
    check "destruct hole reply range" (equal_range reply_range expected)
  in
  let code = "| (😀, _) -> rhs\n" in
  let underscore = String.substr_index_exn code ~pattern:"_" in
  check_one ~code ~cursor_offset:underscore ~expected_offset:underscore;
  let code = "| _ -> 😀\n" in
  let underscore = String.substr_index_exn code ~pattern:"_" in
  let cursor_offset = String.substr_index_exn code ~pattern:"😀" in
  check_one ~code ~cursor_offset ~expected_offset:underscore
;;

let%test_unit "destruct-line preprocessing selects the intended source" =
  Test.run_exn (module Case) ~examples:regression_cases ~f:check_case;
  check_hole_ranges UTF8;
  check_hole_ranges UTF16
;;
