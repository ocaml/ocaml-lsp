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
  Tuple.T2.compare Int.compare Int.compare (dx.line, dy.line) (dx.character, dy.character)
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

let resize_for_edit { TextEdit.range; newText } =
  let lines = String.split_lines newText in
  match lines with
  | [] -> { range with end_ = range.start }
  | several_lines ->
    let end_ =
      let start = range.start in
      let line = start.line + List.length several_lines - 1 in
      let character =
        let last_line_len =
          List.last several_lines |> Option.value_exn |> String.length
        in
        start.character + last_line_len
      in
      { Position.line; character }
    in
    { range with end_ }
;;

let overlaps x y =
  let open Ordering in
  match Position.compare x.start y.end_, Position.compare x.end_ y.start with
  | (Lt | Eq), (Gt | Eq) | (Gt | Eq), (Lt | Eq) -> true
  | _ -> false
;;
