include Types.Position

let zero = create ~line:0 ~character:0
let is_zero (t : t) = t.line = zero.line && t.character = zero.character

let compare t { line; character } =
  match Int.compare t.line line with
  | 0 -> Int.compare t.character character
  | n -> n
;;
