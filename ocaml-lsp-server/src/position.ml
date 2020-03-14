open Import
include Lsp.Types.Position

let of_lexical_position (lex_position : Lexing.position) : t =
  let line = lex_position.pos_lnum - 1 in
  let character = lex_position.pos_cnum - lex_position.pos_bol in
  { line; character }

let ( - ) ({ line; character } : t) (t : t) : t =
  { line = line - t.line; character = character - t.character }

let abs ({ line; character } : t) : t =
  { line = abs line; character = abs character }

let compare ({ line; character } : t) (t : t) : Ordering.t =
  Stdune.Tuple.T2.compare Int.compare Int.compare (line, character)
    (t.line, t.character)

let compare_inclusion (t : t) (r : Range.t) =
  match (compare t r.start, compare t r.end_) with
  | Lt, Lt -> `Outside (abs (r.start - t))
  | Gt, Gt -> `Outside (abs (r.end_ - t))
  | Eq, Lt
  | Gt, Eq
  | Eq, Eq
  | Gt, Lt ->
    `Inside
  | Eq, Gt
  | Lt, Eq
  | Lt, Gt ->
    assert false

let logical position =
  let line = position.line + 1 in
  let col = position.character in
  `Logical (line, col)
