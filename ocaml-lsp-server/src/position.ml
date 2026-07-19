open Import
include Lsp.Types.Position

let to_dyn { line; character } =
  Dyn.record [ "line", Dyn.int line; "character", Dyn.int character ]
;;

let start = { line = 0; character = 0 }

let is_dummy (lp : Lexing.position) =
  lp.pos_lnum = Lexing.dummy_pos.pos_lnum && lp.pos_cnum = Lexing.dummy_pos.pos_cnum
;;

let of_lexical_position (lex_position : Lexing.position) : t option =
  if is_dummy lex_position
  then None
  else (
    let line = lex_position.pos_lnum - 1 in
    let character = lex_position.pos_cnum - lex_position.pos_bol in
    if not (line >= 0 && character >= 0)
    then
      Log.log ~section:"debug" (fun () ->
        Log.msg
          "merlin returned dummy position %a"
          [ "pos_fname", `String lex_position.pos_fname
          ; "pos_lnum", `Int lex_position.pos_lnum
          ; "pos_bol", `Int lex_position.pos_bol
          ; "pos_cnum", `Int lex_position.pos_cnum
          ]);
    let line = max line 0 in
    let character = max character 0 in
    Some { line; character })
;;

let ( - ) ({ line; character } : t) (t : t) : t =
  { line = line - t.line; character = character - t.character }
;;

let abs ({ line; character } : t) : t = { line = abs line; character = abs character }

let compare ({ line; character } : t) (t : t) : Ordering.t =
  match Int.compare line t.line with
  | Eq -> Int.compare character t.character
  | r -> r
;;

let compare_inclusion (t : t) (r : Lsp.Types.Range.t) =
  match compare t r.start, compare t r.end_ with
  | Lt, Lt -> `Outside (abs (r.start - t))
  | Gt, Gt -> `Outside (abs (r.end_ - t))
  | Eq, Lt | Gt, Eq | Eq, Eq | Gt, Lt -> `Inside
  | Eq, Gt | Lt, Eq | Lt, Gt -> assert false
;;

let absolute_offset ~position_encoding text position =
  let zipper = Lsp.Private.String_zipper.of_string text in
  Lsp.Private.String_zipper.goto_position zipper position position_encoding
  |> Lsp.Private.String_zipper.offset
;;

let advance_text ~position_encoding position text =
  let rec loop position offset =
    if offset = String.length text
    then position
    else (
      let decoded = Stdlib.String.get_utf_8_uchar text offset in
      if not (Stdlib.Uchar.utf_decode_is_valid decoded)
      then invalid_arg "Position.advance_text: invalid UTF-8";
      let uchar = Stdlib.Uchar.utf_decode_uchar decoded in
      let byte_length = Stdlib.Uchar.utf_decode_length decoded in
      let position =
        if Stdlib.Uchar.equal uchar (Stdlib.Uchar.of_char '\n')
        then { line = position.line + 1; character = 0 }
        else (
          let character_width =
            match position_encoding with
            | `UTF8 -> byte_length
            | `UTF16 -> Stdlib.Uchar.utf_16_byte_length uchar / 2
          in
          { position with character = position.character + character_width })
      in
      loop position (offset + byte_length))
  in
  loop position 0
;;

let of_offset ~position_encoding text offset =
  if offset < 0 || offset > String.length text
  then invalid_arg "Position.of_offset: offset out of bounds";
  advance_text ~position_encoding start (String.sub text ~pos:0 ~len:offset)
;;

let logical position =
  let line = position.line + 1 in
  let col = position.character in
  `Logical (line, col)
;;
