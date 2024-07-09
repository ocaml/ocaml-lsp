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

type op =
  [ `Goto_line of int
  | `Insert of string
  ]

let test ?(which = `All) mode start operations =
  let results =
    List.fold_left
      operations
      ~init:[ `Hide, start ]
      ~f:(fun acc (op : [ op | `Hide of op ]) ->
        let final =
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
        final :: acc)
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
  [%expect {|
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
  [%expect {|
    line 0: "|foo" |}];
  test `String foo [ `Insert "a" ];
  [%expect {|
    line 0: "|afoo" |}];
  test `String foo [ `Insert "a"; `Insert "b" ];
  [%expect {|
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
  let t = String_zipper.of_string "foo\nbar\nxxx" in
  let t = String_zipper.goto_line t 1 in
  let t' = String_zipper.goto_line t 2 in
  let t = String_zipper.drop_until t t' in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect {|
    "foo\n|xxx" |}];
  let t = String_zipper.of_string "foo\nbar\n" in
  let t = String_zipper.goto_line t 2 in
  let t = String_zipper.drop_until t t in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect {|
    "foo\nbar\n|" |}];
  let t = String_zipper.of_string "123\n" in
  let t = String_zipper.goto_line t 1 in
  let t = String_zipper.drop_until t t in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect {| "123\n|" |}]
;;

let%expect_test "squashing" =
  let str = "foo\nbar" in
  let t = String_zipper.of_string str in
  let t = String_zipper.goto_line t 1 in
  let t, str' = String_zipper.squash t in
  assert (String.equal str str');
  printfn "squashing: %S" (String_zipper.to_string_debug t);
  [%expect {|
    squashing: "foo\n|bar" |}]
;;

let%expect_test "add buffer between" =
  let str = "foo\nbar" in
  let t = String_zipper.of_string str in
  let t' = String_zipper.goto_line t 1 in
  let b = Buffer.create 0 in
  String_zipper.add_buffer_between b t t';
  printfn "result: %S" (Buffer.contents b);
  [%expect {|
    result: "foo\n" |}]
;;

let%expect_test "drop_until bug" =
  let t = String_zipper.of_string "foo\nbar\nxxx" in
  let t' = String_zipper.goto_line t 10 in
  let t = String_zipper.goto_line t 2 in
  let t = String_zipper.drop_until t t' in
  printfn "%S" (String_zipper.to_string_debug t);
  [%expect {|
    "foo\nbar\n|" |}];
  printfn "abs_pos: %d" (String_zipper.Private.reflect t).abs_pos;
  [%expect {|
    abs_pos: 8 |}]
;;
