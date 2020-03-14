open Import
include Lsp.Types.Range

(* Compares ranges by their lengths*)
let compare_size (x : t) (y : t) =
  let dx = Position.(x.end_ - x.start) in
  let dy = Position.(y.end_ - y.start) in
  Stdune.Tuple.T2.compare Int.compare Int.compare (dx.line, dy.line)
    (dx.character, dy.character)
