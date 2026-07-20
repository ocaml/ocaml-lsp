open Lsp.Types

let%expect_test "replacement ranges preserve trailing newlines" =
  let start = Position.create ~line:2 ~character:5 in
  let range = Range.create ~start ~end_:start in
  let edit = TextEdit.create ~range ~newText:"x\n" in
  let range = Ocaml_lsp_server.Testing.Range.resize_for_edit edit in
  print_endline (Ocaml_lsp_server.Testing.Range.to_string range);
  [%expect {| ((2, 5), (2, 6)) |}]
;;

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
