let%expect_test "snippet text is not escaped" =
  Lsp.Snippet.text "$}\\" |> Lsp.Snippet.to_string |> print_endline;
  [%expect {| $}\ |}]
;;
