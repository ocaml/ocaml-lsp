open Base
open Base_quickcheck
module Array_view = Lsp.Private.Array_view
module Substring = Lsp.Private.Substring

let select selector ~choices = Int.rem (selector land Stdlib.max_int) choices
let check label condition = if not condition then failwith label

let check_string label expected actual =
  if not (String.equal expected actual)
  then failwith (Printf.sprintf "%s: expected %S, got %S" label expected actual)
;;

let check_array label expected actual =
  if not (Array.equal Int.equal expected actual)
  then failwith (Printf.sprintf "%s: arrays differ" label)
;;

let substring_index_from text ~pos needle =
  let rec loop index =
    if index = String.length text
    then None
    else if Char.equal text.[index] needle
    then Some index
    else loop (index + 1)
  in
  loop pos
;;

let substring_rindex_from text ~pos needle =
  let rec loop index =
    if index < 0
    then None
    else if Char.equal text.[index] needle
    then Some index
    else loop (index - 1)
  in
  loop (pos - 1)
;;

let count_newlines text ~pos ~len =
  let stop = pos + len in
  let rec loop index count =
    if index = stop
    then count
    else loop (index + 1) (if Char.equal text.[index] '\n' then count + 1 else count)
  in
  loop pos 0
;;

module Substring_case = struct
  type t =
    { prefix : string
    ; contents : string
    ; suffix : string
    ; other : string
    ; position_selector : int
    ; length_selector : int
    ; needle : char
    }
  [@@deriving quickcheck, sexp_of]
end

let check_substring_case
      { Substring_case.prefix
      ; contents
      ; suffix
      ; other
      ; position_selector
      ; length_selector
      ; needle
      }
  =
  let substring =
    Substring.of_slice
      (prefix ^ contents ^ suffix)
      ~pos:(String.length prefix)
      ~len:(String.length contents)
  in
  let contents_length = String.length contents in
  let position = select position_selector ~choices:(contents_length + 1) in
  let amount = select length_selector ~choices:(contents_length + 3) in
  check "substring length" (Substring.length substring = contents_length);
  check_string "substring contents" contents (Substring.to_string substring);
  let other_substring = Substring.of_string other in
  check
    "substring comparison"
    (Stdlib.compare (Substring.compare substring other_substring) 0
     = Stdlib.compare (String.compare contents other) 0);
  let taken = Int.min position contents_length in
  check_string
    "take"
    (String.sub contents ~pos:0 ~len:taken)
    (Substring.take substring position |> Substring.to_string);
  check_string
    "drop"
    (String.sub contents ~pos:taken ~len:(contents_length - taken))
    (Substring.drop substring position |> Substring.to_string);
  let left, right = Substring.split_at substring position in
  check_string
    "split_at left"
    (String.sub contents ~pos:0 ~len:position)
    (Substring.to_string left);
  check_string
    "split_at right"
    (String.sub contents ~pos:position ~len:(contents_length - position))
    (Substring.to_string right);
  let left, right = Substring.rsplit_at substring position in
  let split = contents_length - position in
  check_string
    "rsplit_at left"
    (String.sub contents ~pos:0 ~len:split)
    (Substring.to_string left);
  check_string
    "rsplit_at right"
    (String.sub contents ~pos:split ~len:position)
    (Substring.to_string right);
  let buffer = Buffer.create contents_length in
  Substring.add_buffer substring buffer;
  check_string "add_buffer" contents (Buffer.contents buffer);
  let destination = Bytes.make (contents_length + 2) '!' in
  Substring.blit substring ~dst:destination ~dst_pos:1;
  check_string "blit" ("!" ^ contents ^ "!") (Bytes.to_string destination);
  let pieces = [| Substring.of_string prefix; substring; Substring.of_string suffix |] in
  check_string
    "concat"
    (prefix ^ contents ^ suffix)
    (Substring.concat (Array_view.make pieces ~pos:0));
  if contents_length > 0
  then (
    let index = select position_selector ~choices:contents_length in
    check "get_exn" (Char.equal contents.[index] (Substring.get_exn substring index));
    check
      "index_from"
      (Option.equal
         Int.equal
         (substring_index_from contents ~pos:index needle)
         (Substring.index_from substring ~pos:index needle)));
  check
    "rindex_from"
    (Option.equal
       Int.equal
       (substring_rindex_from contents ~pos:position needle)
       (Substring.rindex_from substring ~pos:position needle));
  check
    "rindex"
    (Option.equal
       Int.equal
       (substring_rindex_from contents ~pos:contents_length needle)
       (Substring.rindex substring needle));
  let right_move = Substring.move_right substring ~pos:position ~len:amount in
  let right_consumed = Int.min amount (contents_length - position) in
  check "move_right consumed" (right_move.consumed = right_consumed);
  check
    "move_right newlines"
    (right_move.newlines = count_newlines contents ~pos:position ~len:right_consumed);
  let left_move = Substring.move_left substring ~pos:position ~len:amount in
  let left_consumed = Int.min amount position in
  check "move_left consumed" (left_move.consumed = left_consumed);
  check
    "move_left newlines"
    (left_move.newlines
     = count_newlines contents ~pos:(position - left_consumed) ~len:left_consumed)
;;

module Array_view_case = struct
  type t =
    { prefix : int list
    ; contents : int list
    ; suffix : int list
    ; other : int list
    ; position_selector : int
    ; length_selector : int
    ; replacement : int
    }
  [@@deriving quickcheck, sexp_of]
end

let common_suffix_length left right =
  let rec loop count =
    if count = Array.length left || count = Array.length right
    then count
    else if
      Int.equal
        left.(Array.length left - count - 1)
        right.(Array.length right - count - 1)
    then loop (count + 1)
    else count
  in
  loop 0
;;

let check_array_view_case
      { Array_view_case.prefix
      ; contents
      ; suffix
      ; other
      ; position_selector
      ; length_selector
      ; replacement
      }
  =
  let prefix = Array.of_list prefix in
  let expected = Array.of_list contents in
  let suffix = Array.of_list suffix in
  let backing = Array.concat [ prefix; expected; suffix ] in
  let view =
    Array_view.make backing ~pos:(Array.length prefix) ~len:(Array.length expected)
  in
  check "array view length" (Array_view.length view = Array.length expected);
  check
    "array view emptiness"
    (Bool.equal (Array_view.is_empty view) (Array.is_empty expected));
  check_array "array view copy" expected (Array_view.copy view);
  check
    "array view fold"
    (List.equal
       Int.equal
       (Array.to_list expected)
       (Array_view.fold_left view ~init:[] ~f:(fun result value -> value :: result)
        |> List.rev));
  let visited = ref [] in
  Array_view.iteri view ~f:(fun index value -> visited := (index, value) :: !visited);
  check
    "array view iteri"
    (List.equal
       (fun (left_index, left_value) (right_index, right_value) ->
          Int.equal left_index right_index && Int.equal left_value right_value)
       (List.mapi contents ~f:(fun index value -> index, value))
       (List.rev !visited));
  Array.iteri expected ~f:(fun index value ->
    check "array view get" (Int.equal value (Array_view.get view index));
    check
      "array view backing position"
      (Array_view.backing_array_pos view index = Array.length prefix + index));
  let position = select position_selector ~choices:(Array.length expected + 1) in
  let remaining = Array.length expected - position in
  let length = select length_selector ~choices:(remaining + 1) in
  let subview = Array_view.sub view ~pos:position ~len:length in
  check_array
    "array view sub"
    (Array.sub expected ~pos:position ~len:length)
    (Array_view.copy subview);
  let other = Array.of_list other in
  let other_view = Array_view.make other ~pos:0 in
  check
    "common suffix length"
    (Array_view.common_suffix_len view other_view = common_suffix_length expected other);
  let destination = Array.create ~len:(Array.length expected + 2) 0 in
  Array_view.blit view destination ~pos:1;
  check_array "array view blit" (Array.concat [ [| 0 |]; expected; [| 0 |] ]) destination;
  if not (Array.is_empty expected)
  then (
    let index = select position_selector ~choices:(Array.length expected) in
    let mutable_backing = Array.copy backing in
    let mutable_view =
      Array_view.make
        mutable_backing
        ~pos:(Array.length prefix)
        ~len:(Array.length expected)
    in
    Array_view.set mutable_view index replacement;
    let expected = Array.copy expected in
    expected.(index) <- replacement;
    check_array "array view set" expected (Array_view.copy mutable_view))
;;

let assert_invalid_arg f =
  match f () with
  | exception Invalid_argument _ -> ()
  | exception exn -> raise exn
  | _ -> failwith "expected Invalid_argument"
;;

let%test_unit "slice bounds are checked relative to the slice" =
  let substring = Substring.of_slice "prefix" ~pos:2 ~len:3 in
  assert_invalid_arg (fun () -> Substring.get_exn substring (-1));
  let backing = Array.init 8 ~f:Fn.id in
  assert_invalid_arg (fun () -> Array_view.make backing ~pos:0 ~len:(-1));
  let view = Array_view.make backing ~pos:2 ~len:3 in
  assert_invalid_arg (fun () -> Array_view.get view (-1));
  assert_invalid_arg (fun () -> Array_view.set view (-1) 0);
  assert_invalid_arg (fun () -> Array_view.sub view ~pos:(-1) ~len:1);
  assert_invalid_arg (fun () -> Array_view.sub view ~pos:2 ~len:2)
;;

let%test_unit "substring operations agree with strings" =
  Test.run_exn (module Substring_case) ~f:check_substring_case
;;

let%test_unit "array view operations agree with arrays" =
  Test.run_exn (module Array_view_case) ~f:check_array_view_case
;;
