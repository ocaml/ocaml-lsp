let%expect_test "snippet text is not escaped" =
  Lsp.Snippet.text "$}\\" |> Lsp.Snippet.to_string |> print_endline;
  [%expect {| $}\ |}]
;;

let%expect_test "snippet choices escape metacharacters" =
  Lsp.Snippet.choice [ "$"; "}"; "\\"; ","; "|" ]
  |> Lsp.Snippet.to_string
  |> print_endline;
  [%expect {snippet| ${1|\$,\},\\,\,,\||} |snippet}]
;;

let%expect_test "the final tab stop is renumbered" =
  Lsp.Snippet.tabstop 0 |> Lsp.Snippet.to_string |> print_endline;
  [%expect {| $1 |}]
;;
