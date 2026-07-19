open Import
include Lsp.Types.Range

let compare (x : t) (y : t) =
  match Position.compare x.start y.start with
  | (Lt | Gt) as r -> r
  | Ordering.Eq -> Position.compare x.end_ y.end_
;;

let to_dyn { start; end_ } =
  Dyn.record [ "start", Position.to_dyn start; "end_", Position.to_dyn end_ ]
;;

let contains (x : t) (y : t) =
  let open Ordering in
  match Position.compare x.start y.start, Position.compare x.end_ y.end_ with
  | (Lt | Eq), (Gt | Eq) -> true
  | _ -> false
;;

(* Compares ranges by their lengths*)
let compare_size (x : t) (y : t) =
  let dx = Position.(x.end_ - x.start) in
  let dy = Position.(y.end_ - y.start) in
  Poly.compare (dx.line, dx.character) (dy.line, dy.character)
;;

let first_line =
  let start = { Position.line = 0; character = 0 } in
  let end_ = { Position.line = 1; character = 0 } in
  { start; end_ }
;;

let of_loc_opt (loc : Loc.t) : t option =
  let open Option.O in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  { start; end_ }
;;

let of_loc (loc : Loc.t) : t = of_loc_opt loc |> Option.value ~default:first_line

let resize_for_edit ~position_encoding { TextEdit.range; newText } =
  let rec position_after_text (position : Position.t) offset =
    if offset = String.length newText
    then position
    else (
      let decoded = Stdlib.String.get_utf_8_uchar newText offset in
      if not (Stdlib.Uchar.utf_decode_is_valid decoded)
      then invalid_arg "Range.resize_for_edit: invalid UTF-8";
      let uchar = Stdlib.Uchar.utf_decode_uchar decoded in
      let byte_length = Stdlib.Uchar.utf_decode_length decoded in
      let position =
        if Stdlib.Uchar.equal uchar (Stdlib.Uchar.of_char '\n')
        then { Position.line = position.line + 1; character = 0 }
        else (
          let character_width =
            match position_encoding with
            | `UTF8 -> byte_length
            | `UTF16 -> Stdlib.Uchar.utf_16_byte_length uchar / 2
          in
          { position with character = position.character + character_width })
      in
      position_after_text position (offset + byte_length))
  in
  let end_ = position_after_text range.start 0 in
  { range with end_ }
;;

let overlaps x y =
  let open Ordering in
  match Position.compare x.start y.end_, Position.compare x.end_ y.start with
  | (Lt | Eq), (Gt | Eq) | (Gt | Eq), (Lt | Eq) -> true
  | _ -> false
;;

let to_string t =
  sprintf
    "((%d, %d), (%d, %d))"
    t.start.line
    t.start.character
    t.end_.line
    t.end_.character
;;
