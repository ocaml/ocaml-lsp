open Lsp.Types

let%expect_test "replacement ranges preserve trailing newlines" =
  let start = Position.create ~line:2 ~character:5 in
  let range = Range.create ~start ~end_:start in
  let edit = TextEdit.create ~range ~newText:"x\n" in
  let range = Ocaml_lsp_server.Testing.Range.resize_for_edit edit in
  print_endline (Ocaml_lsp_server.Testing.Range.to_string range);
  [%expect {| ((2, 5), (2, 6)) |}]
;;

let%expect_test "replacement ranges use UTF-16 character units" =
  let start = Position.create ~line:2 ~character:5 in
  let range = Range.create ~start ~end_:start in
  let edit = TextEdit.create ~range ~newText:"😀" in
  let range = Ocaml_lsp_server.Testing.Range.resize_for_edit edit in
  print_endline (Ocaml_lsp_server.Testing.Range.to_string range);
  [%expect {| ((2, 5), (2, 9)) |}]
;;

let%expect_test "document-symbol selection range relationships" =
  let range start_line start_character end_line end_character =
    let start = Position.create ~line:start_line ~character:start_character in
    let end_ = Position.create ~line:end_line ~character:end_character in
    Range.create ~start ~end_
  in
  let compare_position (x : Position.t) (y : Position.t) =
    Stdlib.compare (x.line, x.character) (y.line, y.character)
  in
  let full_range = range 1 2 3 8 in
  let relation = function
    | None -> "ghost"
    | Some (selection_range : Range.t) ->
      if compare_position selection_range.start selection_range.end_ > 0
      then "invalid"
      else if Ocaml_lsp_server.Testing.Range.contains full_range selection_range
      then "contained"
      else (
        let start =
          if compare_position full_range.start selection_range.start >= 0
          then full_range.start
          else selection_range.start
        in
        let end_ =
          if compare_position full_range.end_ selection_range.end_ <= 0
          then full_range.end_
          else selection_range.end_
        in
        match compare_position start end_ with
        | n when n < 0 -> "overlap"
        | 0 -> "touch"
        | _ -> "disjoint")
  in
  let print label selection_range =
    Printf.printf "%s: %s\n" label (relation selection_range)
  in
  print "contained" (Some (range 1 4 2 6));
  print "contained empty" (Some (range 2 4 2 4));
  print "overlap before" (Some (range 0 9 1 5));
  print "overlap after" (Some (range 3 4 4 1));
  print "enclosing" (Some (range 0 0 4 0));
  print "touch before" (Some (range 0 0 1 2));
  print "touch after" (Some (range 3 8 4 0));
  print "disjoint before" (Some (range 0 0 1 1));
  print "disjoint after" (Some (range 3 9 4 0));
  print "ghost" None;
  print "reversed" (Some (range 2 6 2 4));
  [%expect
    {|
    contained: contained
    contained empty: contained
    overlap before: overlap
    overlap after: overlap
    enclosing: overlap
    touch before: touch
    touch after: touch
    disjoint before: disjoint
    disjoint after: disjoint
    ghost: ghost
    reversed: invalid
    |}]
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
