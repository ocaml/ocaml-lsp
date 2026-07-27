let%expect_test "snippet text is not escaped" =
  Lsp.Snippet.text "$}\\" |> Lsp.Snippet.to_string |> print_endline;
  [%expect {| $}\ |}]
;;

let%expect_test "snippet choices double escape metacharacters" =
  Lsp.Snippet.choice [ "$"; "}"; "\\"; ","; "|" ]
  |> Lsp.Snippet.to_string
  |> print_endline;
  [%expect {snippet| ${1|\\$,\\},\\,\,,\||} |snippet}]
;;
