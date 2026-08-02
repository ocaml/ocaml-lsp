open Lsp.Types
module Position = Lsp.Position
module Range = Lsp.Range

let resize newText =
  let start = Position.create ~line:2 ~character:5 in
  let range = Range.create ~start ~end_:start in
  let edit = TextEdit.create ~range ~newText in
  Range.resize_for_edit edit
;;

let%expect_test "resize_for_edit preserves trailing newlines" =
  print_endline (Range.to_string (resize "x\n"));
  print_endline (Range.to_string (resize ""));
  print_endline (Range.to_string (resize "x"));
  print_endline (Range.to_string (resize "ab\ncd"));
  print_endline (Range.to_string (resize "a\nb\n"));
  print_endline (Range.to_string (resize "ab\n\n"));
  print_endline (Range.to_string (resize "\n"));
  [%expect
    {|
    ((2, 5), (2, 6))
    ((2, 5), (2, 5))
    ((2, 5), (2, 6))
    ((2, 5), (3, 2))
    ((2, 5), (3, 1))
    ((2, 5), (3, 0))
    ((2, 5), (2, 5))
    |}]
;;

let%expect_test "resize_for_edit uses UTF-16 character units" =
  print_endline (Range.to_string (resize "😀"));
  [%expect {| ((2, 5), (2, 9)) |}]
;;
