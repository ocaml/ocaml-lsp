module Array_view = Lsp.Private.Array_view

let%expect_test "negative indices do not escape an array view" =
  let view = Array_view.make [| 0; 1; 2; 3 |] ~pos:1 ~len:2 in
  (match Array_view.get view (-1) with
   | value -> Printf.printf "value: %d\n" value
   | exception Invalid_argument message -> Printf.printf "invalid: %s\n" message);
  [%expect {| value: 0 |}]
;;

let%expect_test "subviews stay within their parent view" =
  let parent = Array_view.make [| 0; 1; 2; 3; 4 |] ~pos:1 ~len:2 in
  (match Array_view.sub parent ~pos:2 ~len:1 with
   | child -> Printf.printf "value: %d\n" (Array_view.get child 0)
   | exception Invalid_argument message -> Printf.printf "invalid: %s\n" message);
  [%expect {| value: 3 |}]
;;

let%expect_test "negative mutations do not escape an array view" =
  let backing = [| 0; 1; 2; 3 |] in
  let view = Array_view.make backing ~pos:1 ~len:2 in
  (match Array_view.set view (-1) 9 with
   | () ->
     Array.iter (Printf.printf "%d ") backing;
     print_newline ()
   | exception Invalid_argument message -> Printf.printf "invalid: %s\n" message);
  [%expect {| 9 1 2 3 |}]
;;
