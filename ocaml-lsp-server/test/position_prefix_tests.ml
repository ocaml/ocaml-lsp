open Ocaml_lsp_server
open Testing
open! Import

let%expect_test "varible in labelled pararm" =
  let document =
    "let map = ListLabels.map\n\nlet _ = map ~f:Int.abs\n"
    |> Testing.Merlin_kernel.Msource.make
  in
  
  let position = `Logical (3, 22) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  
  print_endline prefix;
  [%expect "Int.abs"]
;;

let%expect_test "labelled pararm" =
  let document =
    "let mem = ListLabels.mem\n\nlet _ = mem ~se"
    |> Testing.Merlin_kernel.Msource.make
  in

  let position = `Logical (3, 15) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  
  Printf.printf "prefix:'%s' " prefix;
  [%expect "prefix:'~se'"]
;;

let%expect_test "correctly handle typed hole for code action" =
  let document =
    "let x = _"
    |> Testing.Merlin_kernel.Msource.make
  in
  let position = `Logical (1, 9) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  
  Printf.printf "prefix:'%s' " prefix;
  [%expect "prefix:'_'"]
;;

let%expect_test "complete at infix" =
  let document =
    "let x = 1|>."
    |> Testing.Merlin_kernel.Msource.make
  in
  let position = `Logical (1, 11) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  
  Printf.printf "prefix:'%s' " prefix;
  [%expect "prefix:'|>'"]
;;

let%expect_test "complete at arbitrary position" =
  let document =
    "Strin.func"
    |> Testing.Merlin_kernel.Msource.make
  in
  let position = `Logical (1, 5) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  (*let prefix_old = Compl.prefix_of_position_old ~short_path:false document position in*)
  
  Printf.printf "prefix:'%s' " prefix;
  (*Printf.printf "prefix_old:'%s' " prefix_old;*)
  [%expect "prefix:'Strin'"]
;;

let%expect_test "completion prefix multiple dots test" =
  let document = "[1;2]|>Core.List.ma\n" |> Testing.Merlin_kernel.Msource.make in
  let position = `Logical (1, 19) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  print_endline prefix;
  [%expect "Core.List.ma"]

let%expect_test "completion prefix touching infix test" =
  let document = "[1;2]|>List.ma\n" |> Testing.Merlin_kernel.Msource.make in
  let position = `Logical (1, 14) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  print_endline prefix;
  [%expect "List.ma"]

let%expect_test "completion prefix dot infix test" =
  let document = "[1;2]|>.List.ma\n" |> Testing.Merlin_kernel.Msource.make in
  let position = `Logical (1, 15) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  print_endline prefix;
  [%expect "List.ma"]

let%expect_test "completion prefix with space test" =
  let document = "[1;2] |> List.ma\n" |> Testing.Merlin_kernel.Msource.make in
  let position = `Logical (1, 16) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  print_endline prefix;
  [%expect "List.ma"]

let%expect_test "short path prefix" =
  let document = "[1;2] |> Core.List.ma\n" |> Testing.Merlin_kernel.Msource.make in
  let position = `Logical (1, 22) in
  let prefix = Compl.prefix_of_position ~short_path:true document position in
  print_endline prefix;
  [%expect "ma"]

let%expect_test "Space in dot chain" =
  let document = "[1;2] |> Core. List. ma\n" |> Testing.Merlin_kernel.Msource.make in
  let position = `Logical (1, 23) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  print_endline prefix;
  [%expect "Core.List.ma"]

let%expect_test "newline in dot chain" =
  let document = "[1;2] |> Core.\nList.\nma\n" |> Testing.Merlin_kernel.Msource.make in
  let position = `Logical (3, 2) in
  let prefix = Compl.prefix_of_position ~short_path:false document position in
  print_endline prefix;
  [%expect "Core.List.ma"]

