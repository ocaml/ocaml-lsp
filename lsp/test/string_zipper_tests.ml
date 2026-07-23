open Stdune
module String_zipper = Lsp.Private.String_zipper
module Substring = Lsp.Private.Substring

let to_dyn { String_zipper.Private.left; rel_pos; current; right; line; abs_pos } =
  let open Dyn in
  let sub x = string (Substring.to_string x) in
  let subs = list sub in
  record
    [ "left", subs left
    ; "rel_pos", int rel_pos
    ; "abs_pos", int abs_pos
    ; "current", sub current
    ; "right", subs right
    ; "line", int line
    ]
;;

let check_invariants zipper =
  let state = String_zipper.Private.reflect zipper in
  let fail invariant =
    failwith (Printf.sprintf "%s: %s" invariant (state |> to_dyn |> Dyn.to_string))
  in
  let check invariant condition = if not condition then fail invariant in
  let substring_length total substring = total + Substring.length substring in
  let left_length = List.fold_left state.left ~init:0 ~f:substring_length in
  let current_length = Substring.length state.current in
  check "abs_pos differs from the left chunks" (state.abs_pos = left_length);
  check
    "rel_pos is outside the current chunk"
    (state.rel_pos >= 0 && state.rel_pos <= current_length);
  check
    "cursor is at the end of a non-final chunk"
    (state.rel_pos <> current_length || List.is_empty state.right);
  let offset = left_length + state.rel_pos in
  check "offset differs from the cursor" (String_zipper.offset zipper = offset);
  let text = String_zipper.to_string zipper in
  check "offset is outside the text" (offset >= 0 && offset <= String.length text);
  let rec count_newlines position count =
    if position = offset
    then count
    else
      count_newlines
        (position + 1)
        (if Char.equal text.[position] '\n' then count + 1 else count)
  in
  check "line differs from the text prefix" (state.line = count_newlines 0 0)
;;

let checked zipper =
  check_invariants zipper;
  zipper
;;

type op =
  [ `Goto_line of int
  | `Insert of string
  ]

let test ?(which = `All) mode start operations =
  check_invariants start;
  let results =
    List.fold_left
      operations
      ~init:[ `Hide, start ]
      ~f:(fun acc (op : [ op | `Hide of op ]) ->
        let display, result =
          let _, last = List.hd acc in
          let commit_op op =
            match op with
            | `Insert s -> String_zipper.insert last s
            | `Goto_line g -> String_zipper.goto_line last g
          in
          match op with
          | `Hide op -> `Hide, commit_op op
          | #op as x -> `Show, commit_op x
        in
        check_invariants result;
        (display, result) :: acc)
    |> List.rev
    |> List.tl
  in
  let results =
    match which with
    | `All -> results
    | `Last -> [ List.rev results |> List.hd ]
  in
  List.filter_map results ~f:(fun (display, res) ->
    match display with
    | `Hide -> None
    | `Show -> Some res)
  |> List.iter ~f:(fun res ->
    let res =
      match mode with
      | `Dyn -> String_zipper.Private.reflect res |> to_dyn |> Dyn.to_string
      | `String ->
        let line = (String_zipper.Private.reflect res).line in
        Printf.sprintf "line %d: %S" line (String_zipper.to_string_debug res)
    in
    Printf.printf "%s\n" res)
;;

let%expect_test "goto line" =
  let foo = String_zipper.of_string "foo\nX\nY" in
  test `String foo [ `Goto_line 0 ];
  [%expect {| line 0: "|foo\nX\nY" |}];
  test
    `String
    foo
    [ `Goto_line 0
    ; `Goto_line 1
    ; `Goto_line 2
    ; `Goto_line 3
    ; `Goto_line 2
    ; `Goto_line 1
    ; `Goto_line 0
    ; `Goto_line 0
    ];
  [%expect
    {|
    line 0: "|foo\nX\nY"
    line 1: "foo\n|X\nY"
    line 2: "foo\nX\n|Y"
    line 2: "foo\nX\nY|"
    line 2: "foo\nX\n|Y"
    line 1: "foo\n|X\nY"
    line 0: "|foo\nX\nY"
    line 0: "|foo\nX\nY" |}];
  test `String (String_zipper.of_string "") [ `Goto_line 100; `Goto_line 0 ];
  [%expect
    {|
    line 0: "|"
    line 0: "|" |}];
  test `String foo [ `Insert "baz"; `Goto_line 1; `Insert "1" ];
  [%expect
    {|
    line 0: "|bazfoo\nX\nY"
    line 1: "bazfoo\n|X\nY"
    line 1: "bazfoo\n|1X\nY" |}]
;;

let%expect_test "insertions" =
  let foo = String_zipper.of_string "foo" in
  test `String foo [ `Insert "" ];
  [%expect
    {|
    line 0: "|foo" |}];
  test `String foo [ `Insert "a" ];
  [%expect
    {|
    line 0: "|afoo" |}];
  test `String foo [ `Insert "a"; `Insert "b" ];
  [%expect
    {|
    line 0: "|afoo"
    line 0: "|bafoo" |}]
;;

let%expect_test "mixed insert goto" =
  let foo = String_zipper.of_string "foo" in
  test `String foo [ `Insert "XXX"; `Insert "YYY"; `Insert "zzz" ];
  [%expect
    {|
    line 0: "|XXXfoo"
    line 0: "|YYYXXXfoo"
    line 0: "|zzzYYYXXXfoo" |}]
;;

let%expect_test "drop_until" =
  let t = String_zipper.of_string "foo\nbar\nxxx" |> checked in
  let t = String_zipper.goto_line t 1 |> checked in
  let t' = String_zipper.goto_line t 2 |> checked in
  let t = String_zipper.drop_until t t' |> checked in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect
    {|
    "foo\n|xxx" |}];
  let t = String_zipper.of_string "foo\nbar\n" |> checked in
  let t = String_zipper.goto_line t 2 |> checked in
  let t = String_zipper.drop_until t t |> checked in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect
    {|
    "foo\nbar\n|" |}];
  let t = String_zipper.of_string "123\n" |> checked in
  let t = String_zipper.goto_line t 1 |> checked in
  let t = String_zipper.drop_until t t |> checked in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect {| "123\n|" |}]
;;

let%expect_test "squashing" =
  let str = "foo\nbar" in
  let t = String_zipper.of_string str |> checked in
  let t = String_zipper.goto_line t 1 |> checked in
  let t, str' = String_zipper.squash t in
  check_invariants t;
  assert (String.equal str str');
  printfn "squashing: %S" (String_zipper.to_string_debug t);
  [%expect
    {|
    squashing: "foo\n|bar" |}]
;;

let%expect_test "add buffer between" =
  let str = "foo\nbar" in
  let t = String_zipper.of_string str |> checked in
  let t' = String_zipper.goto_line t 1 |> checked in
  let b = Buffer.create 0 in
  String_zipper.add_buffer_between b t t';
  printfn "result: %S" (Buffer.contents b);
  [%expect
    {|
    result: "foo\n" |}]
;;

let%expect_test "drop_until bug" =
  let t = String_zipper.of_string "foo\nbar\nxxx" |> checked in
  let t' = String_zipper.goto_line t 10 |> checked in
  let t = String_zipper.goto_line t 2 |> checked in
  let t = String_zipper.drop_until t t' |> checked in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect
    {|
    "foo\nbar\n|" |}];
  printfn "abs_pos: %d" (String_zipper.Private.reflect t).abs_pos;
  [%expect {| abs_pos: 0 |}]
;;
