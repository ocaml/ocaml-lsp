include Types.Position

let zero = create ~line:0 ~character:0
let is_zero (t : t) = t.line = zero.line && t.character = zero.character

let compare t { line; character } =
  match Int.compare t.line line with
  | 0 -> Int.compare t.character character
  | n -> n
;;

let min x y = if compare x y <= 0 then x else y
let max x y = if compare x y >= 0 then x else y
let to_logical t = t.line + 1, t.character
let of_logical ~line ~character = create ~line:(line - 1) ~character
