module Array_view = Lsp.Private.Array_view

let%expect_test "negative indices do not escape an array view" =
  let view = Array_view.make [| 0; 1; 2; 3 |] ~pos:1 ~len:2 in
  (match Array_view.get view (-1) with
   | value -> Printf.printf "value: %d\n" value
   | exception Invalid_argument message -> Printf.printf "invalid: %s\n" message);
  [%expect {| value: 0 |}]
;;
