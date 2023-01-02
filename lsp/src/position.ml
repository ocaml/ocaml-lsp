include Types.Position

let zero = create ~line:0 ~character:0

let subtract x y =
  let line = x.line - y.line in
  let character = if line = 0 then x.character - y.character else x.character in
  create ~character ~line

let is_zero (t : t) = t.line = zero.line && t.character = zero.character
