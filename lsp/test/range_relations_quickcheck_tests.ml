open Base
open Base_quickcheck
module Position = Lsp.Position
module Range = Lsp.Range

let line_width = 17
let maximum_offset = 256
let select selector = Int.rem (selector land Stdlib.max_int) (maximum_offset + 1)

let position offset =
  Position.create ~line:(offset / line_width) ~character:(Int.rem offset line_width)
;;

let ordered first second = if first <= second then first, second else second, first

let range start_selector end_selector =
  let start, end_ = ordered (select start_selector) (select end_selector) in
  Range.create ~start:(position start) ~end_:(position end_), start, end_
;;

let check label condition = if not condition then failwith label

let equal_position (left : Position.t) (right : Position.t) =
  left.line = right.line && left.character = right.character
;;

let equal_range (left : Range.t) (right : Range.t) =
  equal_position left.start right.start && equal_position left.end_ right.end_
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
    (Int.equal
       (Position.compare point first.start)
       (Int.compare point_offset first_start));
  check
    "position minimum"
    (equal_position
       (Position.min point first.start)
       (position (Int.min point_offset first_start)));
  check
    "position maximum"
    (equal_position
       (Position.max point first.start)
       (position (Int.max point_offset first_start)));
  let expected_range_order =
    match Int.compare first_start second_start with
    | 0 -> Int.compare first_end second_end
    | result -> result
  in
  check "range comparison" (Int.equal (Range.compare first second) expected_range_order);
  check
    "range containment"
    (Bool.equal
       (Range.contains first second)
       (first_start <= second_start && second_end <= first_end));
  check
    "position containment with exclusive end"
    (Bool.equal
       (Range.contains_position first point ~inclusive_end:false)
       (first_start <= point_offset && point_offset < first_end));
  check
    "position containment with inclusive end"
    (Bool.equal
       (Range.contains_position first point ~inclusive_end:true)
       (first_start <= point_offset && point_offset <= first_end));
  let expected_overlap =
    Int.max first_start second_start < Int.min first_end second_end
  in
  check
    "range overlap"
    (Bool.equal (Range.overlaps first second ~touching:false) expected_overlap);
  check
    "range overlap symmetry"
    (Bool.equal
       (Range.overlaps first second ~touching:false)
       (Range.overlaps second first ~touching:false));
  let expected_touching = first_start <= second_end && second_start <= first_end in
  check
    "range touching"
    (Bool.equal (Range.overlaps first second ~touching:true) expected_touching);
  let expected_intersection =
    let start = Int.max first_start second_start in
    let end_ = Int.min first_end second_end in
    if start < end_
    then Some (Range.create ~start:(position start) ~end_:(position end_))
    else None
  in
  check
    "range intersection"
    (Option.equal equal_range (Range.intersection first second) expected_intersection)
;;

let regression_cases =
  [ { Case.point = 1; first_start = 0; first_end = 1; second_start = 1; second_end = 2 }
  ; { point = 1; first_start = 1; first_end = 1; second_start = 0; second_end = 2 }
  ; { point = 2; first_start = 0; first_end = 4; second_start = 1; second_end = 3 }
  ]
;;

let%test_unit "position and range operations agree with interval arithmetic" =
  Test.run_exn (module Case) ~examples:regression_cases ~f:check_case
;;
