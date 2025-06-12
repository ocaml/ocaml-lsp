open Async
open Test.Import

let run_test ~title ~message source =
  let src, range = Code_actions.parse_selection source in
  let%map result =
    Code_actions.apply_code_action
      ~diagnostics:[ Diagnostic.create ~message:(`String message) ~range () ]
      title
      src
      range
  in
  Option.iter result ~f:print_string
;;

let mark_test = function
  | `Value -> run_test ~title:"Mark as unused" ~message:"unused value"
  | `Open -> run_test ~title:"Replace with open!" ~message:"unused open"
  | `For_loop_index ->
    run_test ~title:"Mark for-loop index as unused" ~message:"unused for-loop index"
;;

let remove_test = function
  | `Value -> run_test ~title:"Remove unused" ~message:"unused value"
  | `Open -> run_test ~title:"Remove unused open" ~message:"unused open"
  | `Open_bang -> run_test ~title:"Remove unused open!" ~message:"unused open!"
  | `Type -> run_test ~title:"Remove unused type" ~message:"unused type"
  | `Module -> run_test ~title:"Remove unused module" ~message:"unused module"
  | `Case -> run_test ~title:"Remove unused case" ~message:"this match case is unused"
  | `Rec -> run_test ~title:"Remove unused rec" ~message:"unused rec flag"
  | `Constructor ->
    run_test ~title:"Remove unused constructor" ~message:"unused constructor"
;;

let%expect_test "mark value in let" =
  let%map () =
    mark_test
      `Value
      {|
let f =
  let $x$ = 1 in
  0
|}
  in
  [%expect
    {|
    let f =
      let _x = 1 in
      0
    |}]
;;

(* todo *)
let%expect_test "mark value in top level let" =
  let%map () =
    mark_test
      `Value
      {|
let $f$ =
  let x = 1 in
  0
|}
  in
  [%expect
    {|
    let _f =
      let x = 1 in
      0
    |}]
;;

let%expect_test "mark value in match" =
  let%map () =
    mark_test
      `Value
      {|
let f = function
  | $x$ -> 0
|}
  in
  [%expect
    {|
    let f = function
      | _x -> 0
    |}]
;;

let%expect_test "remove value in let" =
  let%map () =
    remove_test
      `Value
      {|
let f =
  let $x$ = 1 in
  0
|}
  in
  [%expect
    {|
    let f =
      0
    |}]
;;

(* todo *)
let%expect_test "remove value in top level let" =
  remove_test
    `Value
    {|
let $f$ =
  let x = 1 in
  0
|}
;;

let%expect_test "mark open" =
  let%map () =
    mark_test
      `Open
      {|
$open M$
|}
  in
  [%expect {| open! M |}]
;;

let%expect_test "mark for loop index" =
  let%map () =
    mark_test
      `For_loop_index
      {|
let () =
  for $i$ = 0 to 10 do
    ()
  done
|}
  in
  [%expect
    {|
    let () =
      for _i = 0 to 10 do
        ()
      done
    |}]
;;

let%expect_test "remove open" =
  let%map () =
    remove_test
      `Open
      {|
open A
$open B$
|}
  in
  [%expect {| open A |}]
;;

let%expect_test "remove open!" =
  remove_test
    `Open_bang
    {|
open A
$open! B$
|}
;;

let%expect_test "remove type" =
  let%map () =
    remove_test
      `Type
      {|
$type t = int$
type s = bool
|}
  in
  [%expect {| type s = bool |}]
;;

let%expect_test "remove module" =
  let%map () =
    remove_test
      `Module
      {|
$module A = struct end$
module B = struct end
|}
  in
  [%expect {| module B = struct end |}]
;;

let%expect_test "remove case" =
  let%map () =
    remove_test
      `Case
      {|
let f = function
 | 0 -> 0
 | $0 -> 1$
|}
  in
  [%expect
    {|
    let f = function
     | 0 -> 0
    |}]
;;

let%expect_test "remove rec flag" =
  let%map () =
    remove_test
      `Rec
      {|
let rec $f$ = 0
|}
  in
  [%expect {| let  f = 0 |}]
;;

let%expect_test "remove constructor" =
  let%map () =
    remove_test
      `Constructor
      {|
type t = A $| B$
|}
  in
  [%expect {| type t = A |}]
;;

let%expect_test "remove constructor" =
  let%map () =
    remove_test
      `Constructor
      {|
type t =
  | A
 $| B$
|}
  in
  [%expect
    {|
    type t =
      | A
    |}]
;;

let%expect_test "remove constructor" =
  let%map () =
    remove_test
      `Constructor
      {|
type t =
 $| A$
 | B
|}
  in
  [%expect
    {|
    type t =

     | B
    |}]
;;

let%expect_test "remove constructor" =
  let%map () =
    remove_test
      `Constructor
      {|
type t =
 $A$
 | B
|}
  in
  [%expect
    {|
    type t =

     | B
    |}]
;;
