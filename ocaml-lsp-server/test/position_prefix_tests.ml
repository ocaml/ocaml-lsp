open Ocaml_lsp_server

(** An extensive set of tests to validation that the prefix_op_position function
    correctly returns prefixes merlin is happy with for all the odd ocaml syntax
    that exists *)

let prefix_test ?(short_path = false) document position =
  let document_source = Testing.Merlin_kernel.Msource.make document in
  let prefix = Testing.Compl.prefix_of_position ~short_path document_source position in
  Printf.printf "%s " prefix
;;

let%expect_test "varible in labelled pararm" =
  prefix_test "let map = ListLabels.map\n\nlet _ = map ~f:Int.abs\n" (`Logical (3, 22));
  [%expect "Int.abs"]
;;

let%expect_test "labelled pararm" =
  prefix_test "let mem = ListLabels.mem\n\nlet _ = mem ~se" (`Logical (3, 15));
  [%expect "~se"]
;;

let%expect_test "completion of enum" =
  prefix_test "match kind with\n| `Va" (`Logical (2, 21));
  [%expect "`Va"]
;;

let%expect_test "labelled pararm" =
  prefix_test "let mem = ListLabels.mem\n\nlet _ = mem ~" (`Logical (3, 13));
  [%expect "~"]
;;

let%expect_test "correctly handle typed hole for code action" =
  prefix_test "let x = _" (`Logical (1, 9));
  [%expect "_"]
;;

let%expect_test "complete at infix" =
  prefix_test "let x = 1|>." (`Logical (1, 11));
  [%expect "|>"]
;;

let%expect_test "complete at arbitrary position" =
  prefix_test "Strin.func" (`Logical (1, 5));
  [%expect "Strin"]
;;

let%expect_test "completion prefix multiple dots test" =
  prefix_test "[1;2]|>Core.List.ma\n" (`Logical (1, 19));
  [%expect "Core.List.ma"]
;;

let%expect_test "completion prefix touching infix test" =
  prefix_test "[1;2]|>List.ma\n" (`Logical (1, 14));
  [%expect "List.ma"]
;;

let%expect_test "completion prefix dot infix test" =
  prefix_test "[1;2]|>.List.ma\n" (`Logical (1, 15));
  [%expect "List.ma"]
;;

let%expect_test "completion against bracket" =
  prefix_test "(List.ma)\n" (`Logical (1, 8));
  [%expect "List.ma"]
;;

let%expect_test "completion prefix with space test" =
  prefix_test "[1;2] |> List.ma\n" (`Logical (1, 16));
  [%expect "List.ma"]
;;

let%expect_test "short path prefix" =
  prefix_test ~short_path:true "[1;2] |> Core.List.ma\n" (`Logical (1, 22));
  [%expect "ma"]
;;

let%expect_test "Space in dot chain" =
  prefix_test "[1;2] |> Other. Thing.Core .List . ma\n" (`Logical (1, 37));
  [%expect "Other.Thing.Core.List.ma"]
;;

let%expect_test "newline in dot chain" =
  prefix_test "[1;2] |> Core.\nList.\nma\n" (`Logical (3, 2));
  [%expect "Core.List.ma"]
;;

let%expect_test "let%lwt thing" =
  prefix_test "let%lwt" (`Logical (1, 7));
  [%expect "let%lwt"]
;;

let%expect_test "let+ thing" =
  prefix_test "let+" (`Logical (1, 4));
  [%expect "let+"]
;;

let%expect_test "let+$% thing" =
  prefix_test "let+$%" (`Logical (1, 6));
  [%expect "let+$%"]
;;
