open Lsp.Types
module Position = Lsp.Position
module Range = Lsp.Range

let resize newText =
  let start = Position.create ~line:2 ~character:5 in
  let range = Range.create ~start ~end_:start in
  let edit = TextEdit.create ~range ~newText in
  Range.resize_for_edit edit
;;

let print_resize newText =
  Printf.printf "%S -> %s\n" newText (Range.to_string (resize newText))
;;

let%expect_test "resize_for_edit drops trailing newlines" =
  List.iter print_resize [ "x\n"; ""; "x"; "ab\ncd"; "a\nb\n"; "ab\n\n"; "\n" ];
  [%expect
    {|
    "x\n" -> ((2, 5), (2, 6))
    "" -> ((2, 5), (2, 5))
    "x" -> ((2, 5), (2, 6))
    "ab\ncd" -> ((2, 5), (3, 2))
    "a\nb\n" -> ((2, 5), (3, 1))
    "ab\n\n" -> ((2, 5), (3, 0))
    "\n" -> ((2, 5), (2, 5))
    |}]
;;

let%expect_test "resize_for_edit uses UTF-16 character units" =
  print_endline (Range.to_string (resize "😀"));
  [%expect {| ((2, 5), (2, 9)) |}]
;;
