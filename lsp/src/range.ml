open Import
include Types.Range

let compare x y =
  match Position.compare x.start y.start with
  | 0 -> Position.compare x.end_ y.end_
  | ordering -> ordering
;;

let is_single_line t = t.start.line = t.end_.line

let contains outer inner =
  Position.compare outer.start inner.start <= 0
  && Position.compare inner.end_ outer.end_ <= 0
;;

let contains_position range position ~inclusive_end =
  Position.compare range.start position <= 0
  &&
  if inclusive_end
  then Position.compare position range.end_ <= 0
  else Position.compare position range.end_ < 0
;;

let intersection x y =
  let start = Position.max x.start y.start in
  let end_ = Position.min x.end_ y.end_ in
  if Position.compare start end_ < 0 then Some { start; end_ } else None
;;

let normalize_selection_range (range : t) ~(selection : t) =
  let selection_is_valid = Position.compare selection.start selection.end_ <= 0 in
  if selection_is_valid && contains range selection
  then selection
  else Option.value (intersection range selection) ~default:range
;;

let overlaps x y ~touching =
  if touching
  then Position.compare x.start y.end_ <= 0 && Position.compare y.start x.end_ <= 0
  else Option.is_some (intersection x y)
;;

let first_line =
  let start = Position.zero in
  let end_ = { Types.Position.line = 1; character = 0 } in
  { start; end_ }
;;

let resize_for_edit { Types.TextEdit.range; newText } =
  let end_ =
    let lines = String.split_on_char ~sep:'\n' newText in
    let start = range.start in
    let line = start.line + List.length lines - 1 in
    let character =
      let last_line_len = List.last_exn lines |> String.length in
      if line = start.line then start.character + last_line_len else last_line_len
    in
    { Position.line; character }
  in
  { range with end_ }
;;

let to_string t =
  Printf.sprintf
    "((%d, %d), (%d, %d))"
    t.start.line
    t.start.character
    t.end_.line
    t.end_.character
;;
