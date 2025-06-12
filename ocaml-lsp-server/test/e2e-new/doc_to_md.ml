open Ocaml_lsp_server.Doc_to_md

let print_doc = function
  | Raw s -> print_endline s
  | Markdown s -> print_endline s
;;

let%expect_test "superscript" =
  let doc = {| 2{^30} |} in
  translate doc |> print_doc;
  [%expect {| 2^{30} |}]
;;

let%expect_test "subscript" =
  let doc = {| a{_b} |} in
  translate doc |> print_doc;
  [%expect {| a\_{b} |}]
;;
