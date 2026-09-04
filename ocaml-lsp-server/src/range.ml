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

let of_loc (loc : Loc.t) : t = of_loc_opt loc |> Option.value ~default:first_line
let contains_loc loc pos = contains_position (of_loc loc) pos ~inclusive_end:true

let clamp_to_source ({ start; end_ } : t) source =
  let clamp position =
    let offset = Msource.get_offset source (Position.logical position) in
    let (`Logical (line, character)) = Msource.get_logical source offset in
    Position.create ~line:(line - 1) ~character
  in
  { start = clamp start; end_ = clamp end_ }
;;
