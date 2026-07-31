open Import
include Lsp.Position

let to_dyn { line; character } =
  Dyn.record [ "line", Dyn.int line; "character", Dyn.int character ]
;;

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
    let line = Int.max line 0 in
    let character = Int.max character 0 in
    Some { line; character })
;;

let logical position =
  let line = position.line + 1 in
  let col = position.character in
  `Logical (line, col)
;;
