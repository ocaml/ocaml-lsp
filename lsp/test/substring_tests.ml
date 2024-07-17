module Substring = Lsp.Private.Substring
module List = ListLabels

let () = Printexc.record_backtrace false
let printf = Printf.printf

let make_sub pre sub post =
  let res = String.concat "" [ pre; sub; post ] |> Substring.of_string in
  let res = Substring.drop res (String.length pre) in
  let res = Substring.take res (String.length sub) in
  assert (sub = Substring.to_string res);
  res
;;

let common_variations sub =
  List.map
    [ "foo", ""; "", "baz"; "a", "b"; "\n", ""; "", "\n" ]
    ~f:(fun (pre, post) ->
      let name = Printf.sprintf "(%S, %S, %S)" pre sub post in
      name, make_sub pre sub post)
;;

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
;;

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
;;

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
;;

let test f sub ~pos ~len =
  let res = f (Substring.of_string sub) ~pos ~len in
  let print { Substring.newlines; consumed } =
    Printf.printf "newlines = %d consumed = %d\n" newlines consumed
  in
  print_endline "[definitive]";
  print res;
  let variations = common_variations sub in
  List.iter variations ~f:(fun (name, sub) ->
    let res' = f sub ~pos ~len in
    if res <> res'
    then (
      printf "[FAIL] %s:\n" name;
      print res'))
;;

let%expect_test "move_left" =
  let test = test Substring.move_left in
  test "foobar" ~pos:3 ~len:2;
  [%expect {|
    [definitive]
    newlines = 0 consumed = 2 |}];
  test "foobar" ~pos:3 ~len:0;
  [%expect {|
    [definitive]
    newlines = 0 consumed = 0 |}];
  test "fo\no\nbar" ~pos:4 ~len:3;
  [%expect {|
    [definitive]
    newlines = 1 consumed = 3 |}];
  test "fo\no\nbar" ~pos:4 ~len:2;
  [%expect {|
    [definitive]
    newlines = 1 consumed = 2 |}];
  test "fo" ~pos:1 ~len:2;
  [%expect {|
    [definitive]
    newlines = 0 consumed = 1 |}]
;;

let%expect_test "move_right" =
  let test = test Substring.move_right in
  test "foobar" ~pos:3 ~len:2;
  [%expect {|
    [definitive]
    newlines = 0 consumed = 2 |}];
  test "foobar" ~pos:3 ~len:0;
  [%expect {|
    [definitive]
    newlines = 0 consumed = 0 |}];
  test "\n\nf" ~pos:2 ~len:3;
  [%expect {|
    [definitive]
    newlines = 0 consumed = 1 |}];
  test "fo\no\nbar" ~pos:4 ~len:2;
  [%expect {|
    [definitive]
    newlines = 1 consumed = 2 |}]
;;

let%expect_test "rindex_from" =
  let test sub pos =
    let char = '\n' in
    let f sub =
      match Substring.rindex_from sub ~pos char with
      | s -> Ok s
      | exception exn -> Error exn
    in
    let res = f (Substring.of_string sub) in
    let print = function
      | Ok None -> print_endline "not found"
      | Ok (Some i) -> printf "%i\n" i
      | Error exn -> printf "exception: %s\n" (Printexc.to_string exn)
    in
    print_endline "[definitive]";
    print res;
    let variations = common_variations sub in
    List.iter variations ~f:(fun (name, sub) ->
      let res' = f sub in
      if res <> res'
      then (
        printf "[FAIL] %s:\n" name;
        print res'))
  in
  test "foo" 0;
  [%expect {|
  [definitive]
  not found |}];
  test "foo" 1;
  [%expect {|
  [definitive]
  not found |}];
  test "\nfoo" 1;
  [%expect {|
    [definitive]
    0 |}];
  test "\nfoo" 2;
  [%expect {|
    [definitive]
    0 |}];
  test "\nfoo" 4;
  [%expect {|
    [definitive]
    0 |}];
  test "\nfoo" 5;
  [%expect
    {|
    [definitive]
    exception: Invalid_argument("Substring.rindex_from: out of bounds") |}];
  test "" 0;
  [%expect {|
    [definitive]
    not found |}];
  test "" 1;
  [%expect
    {|
    [definitive]
    exception: Invalid_argument("Substring.rindex_from: out of bounds") |}]
;;
