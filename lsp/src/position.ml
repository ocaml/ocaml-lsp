include Types.Position

let zero = create ~line:0 ~character:0

let is_zero (t : t) = t.line = zero.line && t.character = zero.character
