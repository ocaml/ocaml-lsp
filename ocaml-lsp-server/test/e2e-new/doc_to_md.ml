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

let%expect_test "table" =
  let doc =
    {| {table {tr {td some content} {td some other content}} {tr {td in another} {td row}}} |}
  in
  translate doc |> print_doc;
  [%expect {|
    | some content | some other content |
    | in another | row | |}]
;;

let%expect_test "table2" =
  let doc = {| 
{t | z    | f  |
   |:-----|---:|
   |  fse |  e | }
 |} in
  translate doc |> print_doc;
  [%expect {|
    | z | f |
    |:-|-:|
    | fse | e | |}]
;;

let%expect_test "problematic_translation" =
  let doc = {| {table {tr {td {ul {li first item} {li second item}}}} } |} in
  translate doc |> print_doc;
  [%expect {|
    |  - first item - second item | |}]
;;

let%expect_test "code_with_output" =
  let doc = {| {@ocaml[foo][output {b foo}]} |} in
  translate doc |> print_doc;
  [%expect {|
    ```ocaml
    foo
    ```
    output **foo** |}]
;;
