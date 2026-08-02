open Import
include Lsp.Range

let to_dyn { start; end_ } =
  Dyn.record [ "start", Position.to_dyn start; "end_", Position.to_dyn end_ ]
;;

let of_loc_opt (loc : Loc.t) : t option =
  let open Option.O in
  let* start = Position.of_lexical_position loc.loc_start in
  let+ end_ = Position.of_lexical_position loc.loc_end in
  { start; end_ }
;;

let of_loc (loc : Loc.t) : t =
  of_loc_opt loc |> Option.value ~default:Lsp.Range.first_line
;;

let resize_for_edit { TextEdit.range; newText } =
  let lines = String.split_lines newText in
  match lines with
  | [] -> { range with end_ = range.start }
  | several_lines ->
    let end_ =
      let start = range.start in
      let line = start.line + List.length several_lines - 1 in
      let character =
        let last_line_len = List.last_exn several_lines |> String.length in
        if line = start.line then start.character + last_line_len else last_line_len
      in
      { Position.line; character }
    in
    { range with end_ }
;;
