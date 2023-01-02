module Substring = Lsp.Private.Substring

let printf = Printf.printf

let%expect_test "split_at" =
  let test sub i =
    let l, r = Substring.split_at sub i in
    printf "l = %S r = %S\n" (Substring.to_string l) (Substring.to_string r)
  in
  let s = Substring.of_string "foo|bar" in
  test s 0;
  [%expect {| l = "" r = "foo|bar" |}];
  test s 7;
  [%expect {| l = "foo|bar" r = "" |}];
  test s 3;
  [%expect {| l = "foo" r = "|bar" |}];
  test s 1;
  [%expect {| l = "f" r = "oo|bar" |}]

let%expect_test "index_from" =
  let test sub pos char =
    match Substring.index_from sub ~pos char with
    | None -> print_endline "Not found"
    | Some pos ->
      printf "drop %d = %S\n" pos (Substring.drop sub pos |> Substring.to_string)
  in
  let s = Substring.of_string "foo|bar" in
  test s 0 '|';
  [%expect {| drop 3 = "|bar" |}];
  test s 3 '|';
  [%expect {| drop 3 = "|bar" |}];
  test s 4 '|';
  [%expect {| Not found |}]

let%expect_test "count_upto_chars_or_newline" =
  let test sub i =
    let { Substring.newline; consumed } =
      Substring.count_upto_chars_or_newline sub i
    in
    printf "newline = %b consumed = %d\n" newline consumed
  in
  let s = Substring.of_string "foo|bar" in
  test s 0;
  [%expect {| newline = false consumed = 0 |}];
  test s 1;
  [%expect {| newline = false consumed = 1 |}];
  test s 20;
  [%expect {| newline = false consumed = 7 |}];
  test (Substring.of_string "foo\nbar") 5;
  [%expect {| newline = true consumed = 3 |}];
  test (Substring.of_string "\nfoo") 5;
  [%expect {| newline = true consumed = 0 |}]

let%expect_test "count_upto_chars_or_newline_backwards" =
  let test sub i =
    let { Substring.newline; consumed } =
      Substring.count_upto_chars_or_newline_backwards sub i
    in
    printf "newline = %b consumed = %d\n" newline consumed
  in
  let s = Substring.of_string "foo|bar" in
  test s 0;
  [%expect {|
    newline = false consumed = 0 |}];
  test s 1;
  [%expect {|
    newline = false consumed = 1 |}];
  test s 20;
  [%expect {|
    newline = false consumed = 7 |}];
  test (Substring.of_string "foo\nbar") 5;
  [%expect {|
    newline = true consumed = 3 |}];
  test (Substring.of_string "") 0;
  [%expect {|
    newline = false consumed = 0 |}];
  test (Substring.of_string "") 1;
  [%expect {|
    newline = false consumed = 0 |}];
  test (Substring.of_string "foo\n") 10;
  [%expect {|
    newline = true consumed = 0 |}]

let%expect_test "rsplit_at" =
  let test sub i =
    let l, r = Substring.rsplit_at sub i in
    printf "%S %S\n" (Substring.to_string l) (Substring.to_string r)
  in
  let s = Substring.of_string "foo|bar" in
  test s 0;
  [%expect {|
    "foo|bar" "" |}];
  test s 4;
  [%expect {|
    "foo" "|bar" |}];
  test s 7;
  [%expect {|
    "" "foo|bar" |}]
