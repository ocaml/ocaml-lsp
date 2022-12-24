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

let%test_module "diff" =
  (module struct
    let test ~from ~to_ =
      let edits = Ocaml_lsp_server.Diff.edit ~from ~to_ in
      let json = `List (List.map Lsp.Types.TextEdit.yojson_of_t edits) in
      Yojson.Safe.pretty_to_string ~std:false json |> print_endline

    let%expect_test "empty strings" =
      test ~from:"" ~to_:"";
      [%expect {|
        [] |}]

    let%expect_test "from empty" =
      test ~from:"" ~to_:"foobar";
      [%expect
        {|
        [
          {
            "newText": "foobar\n",
            "range": {
              "end": { "character": 0, "line": 0 },
              "start": { "character": 0, "line": 0 }
            }
          }
        ] |}]

    let%expect_test "from empty - with newline" =
      test ~from:"" ~to_:"foobar\n";
      [%expect
        {|
        [
          {
            "newText": "foobar\n",
            "range": {
              "end": { "character": 0, "line": 0 },
              "start": { "character": 0, "line": 0 }
            }
          }
        ] |}]

    let%expect_test "to empty" =
      test ~from:"foobar" ~to_:"";
      [%expect
        {|
        [
          {
            "newText": "",
            "range": {
              "end": { "character": 0, "line": 1 },
              "start": { "character": 0, "line": 0 }
            }
          }
        ] |}]

    let%expect_test "no change" =
      test ~from:"foobar" ~to_:"foobar";
      [%expect {|
        [] |}]

    let%expect_test "multiline" =
      test ~from:"foo" ~to_:"baz\nbar\nxx\n";
      [%expect
        {|
        [
          {
            "newText": "baz\nbar\nxx\n",
            "range": {
              "end": { "character": 0, "line": 1 },
              "start": { "character": 0, "line": 0 }
            }
          }
        ] |}]

    let%expect_test "change a character" =
      test ~from:"xxx y xx" ~to_:"xxx z xx";
      [%expect
        {|
        [
          {
            "newText": "xxx z xx\n",
            "range": {
              "end": { "character": 0, "line": 1 },
              "start": { "character": 0, "line": 0 }
            }
          }
        ] |}]

    let%expect_test "delete empty line" =
      test ~from:"xxx\n\nyyy\n" ~to_:"xxx\nyyy\n";
      [%expect
        {|
        [
          {
            "newText": "",
            "range": {
              "end": { "character": 0, "line": 2 },
              "start": { "character": 0, "line": 1 }
            }
          }
        ] |}]
  end)
