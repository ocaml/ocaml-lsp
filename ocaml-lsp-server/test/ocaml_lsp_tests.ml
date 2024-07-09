let%expect_test "eat_message tests" =
  let test e1 e2 expected =
    let result = Ocaml_lsp_server.Diagnostics.equal_message e1 e2 in
    if result = expected then print_endline "[PASS]" else print_endline "[FAIL]"
  in
  test "foo bar" "foo  bar" true;
  [%expect {| [PASS] |}];
  test " foobar" "foobar" true;
  [%expect {| [PASS] |}];
  test "foobar" "foobar " true;
  [%expect {| [PASS] |}];
  test "foobar" "foobar\t" true;
  [%expect {| [PASS] |}];
  test "foobar" "foobar\n" true;
  [%expect {| [PASS] |}];
  test "foobar" "foo bar" false;
  [%expect {| [PASS] |}];
  test "foo bar" "foo Bar" false;
  [%expect {| [PASS] |}]
;;
