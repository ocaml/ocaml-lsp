open Base
open Stdune
open Base_quickcheck
module Position = Lsp.Types.Position
module Range = Lsp.Types.Range
module String_zipper = Lsp.Private.String_zipper
module Substring = Lsp.Private.Substring

type atom =
  | A
  | Space
  | Tab
  | Nul
  | Newline
  | Two_byte
  | Three_byte
  | Four_byte
[@@deriving quickcheck, sexp_of]

type encoding =
  | UTF8
  | UTF16
[@@deriving quickcheck, sexp_of]

(* Integer selectors are mapped to valid positions in the current model. This lets
   Quickcheck generate operation lists without knowing each intermediate state. *)
type operation =
  | Insert of atom list
  | Goto_line of int
  | Goto_position of encoding * int
  | Apply_change of encoding * int * int * atom list
  | Drop_until of encoding * int * int
  | Add_buffer_between of encoding * int * int
  | Goto_end
  | Squash
[@@deriving quickcheck, sexp_of]

module Case = struct
  type t =
    { initial : atom list
    ; operations : operation list
    }
  [@@deriving quickcheck, sexp_of]
end

let string_of_atom = function
  | A -> "a"
  | Space -> " "
  | Tab -> "\t"
  | Nul -> "\000"
  | Newline -> "\n"
  | Two_byte -> "é"
  | Three_byte -> "€"
  | Four_byte -> "😀"
;;

let string_of_atoms atoms = List.map atoms ~f:string_of_atom |> String.concat ~sep:""

let lsp_encoding = function
  | UTF8 -> `UTF8
  | UTF16 -> `UTF16
;;

type cursor =
  { byte_offset : int
  ; line : int
  ; character : int
  }

let position { line; character; byte_offset = _ } = Position.create ~line ~character

let cursors text encoding =
  let text_length = String.length text in
  let rec loop byte_offset line character acc =
    let acc = { byte_offset; line; character } :: acc in
    if byte_offset = text_length
    then List.rev acc
    else (
      let decoded = Stdlib.String.get_utf_8_uchar text byte_offset in
      assert (Stdlib.Uchar.utf_decode_is_valid decoded);
      let uchar = Stdlib.Uchar.utf_decode_uchar decoded in
      let byte_length = Stdlib.Uchar.utf_decode_length decoded in
      if Stdlib.Uchar.equal uchar (Stdlib.Uchar.of_char '\n')
      then loop (byte_offset + byte_length) (line + 1) 0 acc
      else (
        let code_units =
          match encoding with
          | UTF8 -> byte_length
          | UTF16 -> Stdlib.Uchar.utf_16_byte_length uchar / 2
        in
        loop (byte_offset + byte_length) line (character + code_units) acc))
  in
  loop 0 0 0 []
;;

let index selector ~length = Stdlib.Int.rem (selector land Stdlib.max_int) length

let select_cursor text encoding selector =
  let cursors = cursors text encoding |> Array.of_list in
  cursors.(index selector ~length:(Array.length cursors))
;;

let ordered_cursors text encoding first second =
  let first = select_cursor text encoding first in
  let second = select_cursor text encoding second in
  if first.byte_offset <= second.byte_offset then first, second else second, first
;;

let replace text ~start ~stop replacement =
  let length = String.length text in
  Stdlib.String.sub text 0 start
  ^ replacement
  ^ Stdlib.String.sub text stop (length - stop)
;;

let line_starts text =
  let rec loop offset acc =
    if offset = String.length text
    then List.rev acc
    else if Char.equal text.[offset] '\n'
    then loop (offset + 1) ((offset + 1) :: acc)
    else loop (offset + 1) acc
  in
  loop 0 [ 0 ]
;;

let count_newlines_before text stop =
  let rec loop offset count =
    if offset = stop
    then count
    else loop (offset + 1) (if Char.equal text.[offset] '\n' then count + 1 else count)
  in
  loop 0 0
;;

type model =
  { text : string
  ; offset : int
  }

type state =
  { model : model
  ; zipper : String_zipper.t
  }

let fail state message =
  let { model = { text; offset }; zipper } = state in
  failwith
    (Printf.sprintf
       "%s\nmodel: text=%S offset=%d\nzipper: text=%S offset=%d debug=%S"
       message
       text
       offset
       (String_zipper.to_string zipper)
       (String_zipper.offset zipper)
       (String_zipper.to_string_debug zipper))
;;

(* Check the observable result and the representation invariants after every operation. *)
let check state =
  let { model = { text; offset }; zipper } = state in
  let actual_text = String_zipper.to_string zipper in
  if not (String.equal text actual_text) then fail state "text differs";
  if String_zipper.offset zipper <> offset then fail state "offset differs";
  let expected_debug =
    Stdlib.String.sub text 0 offset
    ^ "|"
    ^ Stdlib.String.sub text offset (String.length text - offset)
  in
  if not (String.equal expected_debug (String_zipper.to_string_debug zipper))
  then fail state "debug rendering differs";
  let { String_zipper.Private.left; rel_pos; abs_pos; current; right; line } =
    String_zipper.Private.reflect zipper
  in
  let current_length = Substring.length current in
  if rel_pos < 0 || rel_pos > current_length
  then fail state "relative position is invalid";
  let expected_abs_pos =
    List.fold_left left ~init:0 ~f:(fun total substring ->
      total + Substring.length substring)
  in
  if abs_pos <> expected_abs_pos
  then fail state "absolute position differs from left chunks";
  if abs_pos + rel_pos <> offset then fail state "internal positions differ from offset";
  if rel_pos = current_length && not (List.is_empty right)
  then fail state "cursor is at the end of a non-final chunk";
  let expected_line = count_newlines_before text offset in
  if line <> expected_line then fail state "line differs"
;;

let state_at_position state encoding cursor =
  let zipper =
    String_zipper.goto_position state.zipper (position cursor) (lsp_encoding encoding)
  in
  let state = { model = { state.model with offset = cursor.byte_offset }; zipper } in
  check state;
  state
;;

let apply_operation state = function
  | Insert atoms ->
    let replacement = string_of_atoms atoms in
    let model =
      { state.model with
        text =
          replace
            state.model.text
            ~start:state.model.offset
            ~stop:state.model.offset
            replacement
      }
    in
    { model; zipper = String_zipper.insert state.zipper replacement }
  | Goto_line selector ->
    let starts = line_starts state.model.text |> Array.of_list in
    let line = index selector ~length:(Array.length starts + 2) in
    let offset =
      if line < Array.length starts then starts.(line) else String.length state.model.text
    in
    let model = { state.model with offset } in
    { model; zipper = String_zipper.goto_line state.zipper line }
  | Goto_position (encoding, selector) ->
    let cursor = select_cursor state.model.text encoding selector in
    state_at_position state encoding cursor
  | Apply_change (encoding, first, second, atoms) ->
    let first, second = ordered_cursors state.model.text encoding first second in
    let replacement = string_of_atoms atoms in
    let range = Range.create ~start:(position first) ~end_:(position second) in
    let zipper =
      String_zipper.apply_change state.zipper range (lsp_encoding encoding) ~replacement
    in
    let model =
      { text =
          replace
            state.model.text
            ~start:first.byte_offset
            ~stop:second.byte_offset
            replacement
      ; offset = first.byte_offset
      }
    in
    { model; zipper }
  | Drop_until (encoding, first, second) ->
    let first, second = ordered_cursors state.model.text encoding first second in
    let from = state_at_position state encoding first in
    let until = state_at_position state encoding second in
    let zipper = String_zipper.drop_until from.zipper until.zipper in
    let model =
      { text =
          replace state.model.text ~start:first.byte_offset ~stop:second.byte_offset ""
      ; offset = first.byte_offset
      }
    in
    { model; zipper }
  | Add_buffer_between (encoding, first, second) ->
    let first, second = ordered_cursors state.model.text encoding first second in
    let start = state_at_position state encoding first in
    let stop = state_at_position state encoding second in
    let buffer = Buffer.create 0 in
    String_zipper.add_buffer_between buffer start.zipper stop.zipper;
    let expected =
      Stdlib.String.sub
        state.model.text
        first.byte_offset
        (second.byte_offset - first.byte_offset)
    in
    if not (String.equal expected (Buffer.contents buffer))
    then fail state "buffer contents differ";
    state
  | Goto_end ->
    let model = { state.model with offset = String.length state.model.text } in
    { model; zipper = String_zipper.goto_end state.zipper }
  | Squash ->
    let zipper, text = String_zipper.squash state.zipper in
    if not (String.equal text state.model.text) then fail state "squashed text differs";
    { state with zipper }
;;

let run_case { Case.initial; operations } =
  let text = string_of_atoms initial in
  let initial = { model = { text; offset = 0 }; zipper = String_zipper.of_string text } in
  check initial;
  List.fold_left operations ~init:initial ~f:(fun state operation ->
    let state = apply_operation state operation in
    check state;
    state)
  |> ignore
;;

(* These paths previously corrupted [abs_pos]. Keep them even if the generated
   distribution changes. *)
let regression_cases : Case.t list =
  [ { initial = [ A ]; operations = [ Goto_end; Insert [ A ] ] }
  ; { initial = [ A; Newline; A ]
    ; operations = [ Goto_line 1; Insert [ A ]; Goto_end; Goto_line 0 ]
    }
  ; { initial = [ A ]; operations = [ Drop_until (UTF8, 0, 0) ] }
  ; { initial = [ A; A; A ]; operations = [ Drop_until (UTF16, 1, 2) ] }
  ]
;;

let%expect_test "random operation sequences agree with a string model" =
  Base_quickcheck.Test.run_exn (module Case) ~examples:regression_cases ~f:run_case;
  [%expect {| |}]
;;
