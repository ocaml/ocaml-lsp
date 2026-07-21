type t = int

let of_int x =
  assert (x > 0);
  x
;;

let to_int x = x
let to_dyn = Dyn.int
