open Lsp.Types
module Position = Lsp.Position
module Range = Lsp.Range

let%expect_test "convert an LSP position to a Merlin logical position" =
  let position = Position.create ~line:2 ~character:3 in
  let (`Logical (line, column)) = Ocaml_lsp_server.Testing.Position.logical position in
  Printf.printf
    "LSP position (%d, %d) -> Merlin logical position (%d, %d)\n"
    position.line
    position.character
    line
    column;
  [%expect {| LSP position (2, 3) -> Merlin logical position (3, 3) |}]
;;

let%expect_test "replacement ranges preserve trailing newlines" =
  let start = Position.create ~line:2 ~character:5 in
  let range = Range.create ~start ~end_:start in
  let edit = TextEdit.create ~range ~newText:"x\n" in
  let range = Ocaml_lsp_server.Testing.Range.resize_for_edit edit in
  print_endline (Range.to_string range);
  [%expect {| ((2, 5), (2, 6)) |}]
;;

let%expect_test "replacement ranges use UTF-16 character units" =
  let start = Position.create ~line:2 ~character:5 in
  let range = Range.create ~start ~end_:start in
  let edit = TextEdit.create ~range ~newText:"😀" in
  let range = Ocaml_lsp_server.Testing.Range.resize_for_edit edit in
  print_endline (Range.to_string range);
  [%expect {| ((2, 5), (2, 9)) |}]
;;

let%expect_test "document-symbol selection range relationships" =
  let range start_line start_character end_line end_character =
    let start = Position.create ~line:start_line ~character:start_character in
    let end_ = Position.create ~line:end_line ~character:end_character in
    Range.create ~start ~end_
  in
  let full_range = range 1 2 3 8 in
  let relation = function
    | None -> "ghost"
    | Some (selection_range : Range.t) ->
      if Position.compare selection_range.start selection_range.end_ > 0
      then "invalid"
      else if Range.contains full_range selection_range
      then "contained"
      else (
        let start =
          if Position.compare full_range.start selection_range.start >= 0
          then full_range.start
          else selection_range.start
        in
        let end_ =
          if Position.compare full_range.end_ selection_range.end_ <= 0
          then full_range.end_
          else selection_range.end_
        in
        match Position.compare start end_ with
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

let%expect_test "normalize document-symbol selection ranges" =
  let range start_line start_character end_line end_character =
    let start = Position.create ~line:start_line ~character:start_character in
    let end_ = Position.create ~line:end_line ~character:end_character in
    Range.create ~start ~end_
  in
  let full_range = range 1 2 3 8 in
  let print label selection_range =
    let normalized =
      Ocaml_lsp_server.Testing.Document_symbol.normalize_selection_range
        ~range:full_range
        selection_range
    in
    Printf.printf "%s: %s\n" label (Range.to_string normalized)
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
    contained: ((1, 4), (2, 6))
    contained empty: ((2, 4), (2, 4))
    overlap before: ((1, 2), (1, 5))
    overlap after: ((3, 4), (3, 8))
    enclosing: ((1, 2), (3, 8))
    touch before: ((1, 2), (3, 8))
    touch after: ((1, 2), (3, 8))
    disjoint before: ((1, 2), (3, 8))
    disjoint after: ((1, 2), (3, 8))
    ghost: ((1, 2), (3, 8))
    reversed: ((1, 2), (3, 8))
    |}]
;;

let%expect_test "diagnostic message equality ignores insignificant whitespace" =
  let test left right =
    let relation =
      if Ocaml_lsp_server.Diagnostics.equal_message left right
      then "equal"
      else "different"
    in
    Printf.printf "%S <> %S: %s\n" left right relation
  in
  test "foo bar" "foo  bar";
  test " foobar" "foobar";
  test "foobar" "foobar ";
  test "foobar" "foobar\t";
  test "foobar" "foobar\n";
  test "foobar" "foo bar";
  test "foo bar" "foo Bar";
  [%expect
    {|
    "foo bar" <> "foo  bar": equal
    " foobar" <> "foobar": equal
    "foobar" <> "foobar ": equal
    "foobar" <> "foobar\t": equal
    "foobar" <> "foobar\n": equal
    "foobar" <> "foo bar": different
    "foo bar" <> "foo Bar": different
    |}]
;;
