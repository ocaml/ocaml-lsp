open Base
open Base_quickcheck
module Position = Ocaml_lsp_server.Testing.Position
module Range = Ocaml_lsp_server.Testing.Range

let line_width = 17
let maximum_offset = 256
let select selector = Int.rem (selector land Stdlib.max_int) (maximum_offset + 1)

let position offset =
  Lsp.Types.Position.create
    ~line:(offset / line_width)
    ~character:(Int.rem offset line_width)
;;

let ordered first second = if first <= second then first, second else second, first

let range start_selector end_selector =
  let start, end_ = ordered (select start_selector) (select end_selector) in
  Lsp.Types.Range.create ~start:(position start) ~end_:(position end_), start, end_
;;

let ordering integer =
  if integer < 0 then Stdune.Lt else if integer = 0 then Stdune.Eq else Stdune.Gt
;;

let check label condition = if not condition then failwith label

let equal_position (left : Lsp.Types.Position.t) (right : Lsp.Types.Position.t) =
  left.line = right.line && left.character = right.character
;;

module Case = struct
  type t =
    { point : int
    ; first_start : int
    ; first_end : int
    ; second_start : int
    ; second_end : int
    }
  [@@deriving quickcheck, sexp_of]
end

let check_case { Case.point; first_start; first_end; second_start; second_end } =
  let point_offset = select point in
  let point = position point_offset in
  let first, first_start, first_end = range first_start first_end in
  let second, second_start, second_end = range second_start second_end in
  check
    "position comparison"
    (Poly.equal
       (Position.compare point first.start)
       (ordering (Int.compare point_offset first_start)));
  let expected_difference =
    Lsp.Types.Position.create
      ~line:(point.line - first.start.line)
      ~character:(point.character - first.start.character)
  in
  check
    "position subtraction"
    (equal_position Position.(point - first.start) expected_difference);
  check
    "logical position"
    (Poly.equal (Position.logical point) (`Logical (point.line + 1, point.character)));
  let expected_range_order =
    match Int.compare first_start second_start with
    | 0 -> Int.compare first_end second_end
    | result -> result
  in
  check
    "range comparison"
    (Poly.equal (Range.compare first second) (ordering expected_range_order));
  check
    "range containment"
    (Bool.equal
       (Range.contains first second)
       (first_start <= second_start && second_end <= first_end));
  let expected_overlap = first_start <= second_end && second_start <= first_end in
  check "range overlap" (Bool.equal (Range.overlaps first second) expected_overlap);
  check
    "range overlap symmetry"
    (Bool.equal (Range.overlaps first second) (Range.overlaps second first));
  let expected_size_order =
    Poly.compare
      (first.end_.line - first.start.line, first.end_.character - first.start.character)
      ( second.end_.line - second.start.line
      , second.end_.character - second.start.character )
  in
  check
    "range size comparison"
    (Poly.equal (Range.compare_size first second) (ordering expected_size_order));
  let expected_inclusion, expected_distance =
    if point_offset < first_start
    then
      ( `Outside
      , Lsp.Types.Position.create
          ~line:(first.start.line - point.line)
          ~character:(Int.abs (first.start.character - point.character)) )
    else if point_offset > first_end
    then
      ( `Outside
      , Lsp.Types.Position.create
          ~line:(point.line - first.end_.line |> Int.abs)
          ~character:(point.character - first.end_.character |> Int.abs) )
    else `Inside, Lsp.Types.Position.create ~line:0 ~character:0
  in
  match Position.compare_inclusion point first, expected_inclusion with
  | `Inside, `Inside -> ()
  | `Outside actual, `Outside ->
    check "position inclusion distance" (equal_position actual expected_distance)
  | _ -> failwith "position inclusion"
;;

let regression_cases =
  [ { Case.point = 1; first_start = 0; first_end = 1; second_start = 1; second_end = 2 }
  ; { point = 1; first_start = 1; first_end = 1; second_start = 0; second_end = 2 }
  ; { point = 2; first_start = 0; first_end = 4; second_start = 1; second_end = 3 }
  ]
;;

let%test_unit "position and range operations agree with inclusive query intervals" =
  Test.run_exn (module Case) ~examples:regression_cases ~f:check_case
;;
