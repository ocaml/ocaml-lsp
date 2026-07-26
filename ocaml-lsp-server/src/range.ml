open Import
include Lsp.Types.Range

let compare (x : t) (y : t) =
  let result =
    match Lsp.Position.compare x.start y.start with
    | 0 -> Lsp.Position.compare x.end_ y.end_
    | result -> result
  in
  Ordering.of_int result
;;

let to_dyn { start; end_ } =
  Dyn.record [ "start", Position.to_dyn start; "end_", Position.to_dyn end_ ]
;;

let contains (x : t) (y : t) =
  Lsp.Position.compare x.start y.start <= 0 && Lsp.Position.compare y.end_ x.end_ <= 0
;;

let intersection (x : t) (y : t) =
  let start = Lsp.Position.max x.start y.start in
  let end_ = Lsp.Position.min x.end_ y.end_ in
  if Lsp.Position.compare start end_ < 0 then Some { start; end_ } else None
;;

(* Compares ranges by their lengths*)
let compare_size (x : t) (y : t) =
  let dx = Position.(x.end_ - x.start) in
  let dy = Position.(y.end_ - y.start) in
  Poly.compare (dx.line, dx.character) (dy.line, dy.character)
;;

let first_line =
  let start = Lsp.Position.zero in
  let end_ = { Lsp.Types.Position.line = 1; character = 0 } in
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
        if line = start.line then start.character + last_line_len else last_line_len
      in
      { Position.line; character }
    in
    { range with end_ }
;;

let overlaps x y =
  Lsp.Position.compare x.start y.end_ <= 0 && Lsp.Position.compare y.start x.end_ <= 0
;;

let to_string t =
  sprintf
    "((%d, %d), (%d, %d))"
    t.start.line
    t.start.character
    t.end_.line
    t.end_.character
;;
