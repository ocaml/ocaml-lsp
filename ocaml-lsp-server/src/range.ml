open Import
include Lsp.Types.Range

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
