let extract_test = Code_actions.code_action_test ~title:"Extract expression"

let%expect_test "extract local constant" =
  extract_test
    {|
let f =
  0 + $1$
|};
  [%expect
    {|
    let const_name1 = 1
    let f =
      0 + const_name1
    |}]
;;

let%expect_test "extract function single parameter" =
  extract_test
    {|
let fun_name1 () = ()
let f x =
  $(x * 2)$ + 3
|};
  [%expect
    {|
    let fun_name1 () = ()
    let fun_name2 (x) = x * 2
    let f x =
      fun_name2 x + 3
    |}]
;;

let%expect_test "extract function multiple parameter" =
  extract_test
    {|
let f x =
let y = 0 in
$(x * y)$ + 3
|};
  [%expect
    {|
    let fun_name1 (y) (x) = x * y
    let f x =
    let y = 0 in
    fun_name1 y x + 3
    |}]
;;

let%expect_test "extract function with shadowed parameter" =
  extract_test
    {|
let x = 0
let f x = $x + 1$
|};
  [%expect
    {|
    let x = 0
    let fun_name1 (x) = x + 1
    let f x = fun_name1 x
    |}]
;;

let%expect_test "extract function with bound variable" =
  extract_test
    {|
let x = 0
let y = 1
let f x = $x + y$
|};
  [%expect
    {|
    let x = 0
    let y = 1
    let fun_name1 (x) = x + y
    let f x = fun_name1 x
    |}]
;;

let%expect_test "extract higher order function" =
  extract_test
    {|
let f x =
  $List.map (fun y -> y + 1) x$
|};
  [%expect
    {|
    let fun_name1 (x) = List.map (fun y -> y + 1) x
    let f x =
      fun_name1 x
    |}]
;;

let%expect_test "extract higher order function" =
  extract_test
    {|
let f y =
  $List.map (fun y -> y + 1) y$
|};
  [%expect
    {|
    let fun_name1 (y) = List.map (fun y -> y + 1) y
    let f y =
      fun_name1 y
    |}]
;;

let%expect_test "extract higher order function" =
  extract_test
    {|
let f y =
$List.map (fun y -> y + 1) y$
|};
  [%expect
    {|
    let fun_name1 (y) = List.map (fun y -> y + 1) y
    let f y =
    fun_name1 y
    |}]
;;

let%expect_test "extract higher order function" =
  extract_test
    {|
let f y =
  List.map $(fun y -> y + 1)$ y
|};
  [%expect
    {|
    let fun_name1 = fun y -> y + 1
    let f y =
      List.map fun_name1  y
    |}]
;;

let%expect_test "extract inside let binding" =
  extract_test
    {|
let f y =
  let y = y + 1 in
  $y + 2$
|};
  [%expect
    {|
    let fun_name1 (y) = y + 2
    let f y =
      let y = y + 1 in
      fun_name1 y
    |}]
;;

let%expect_test "extract free variable" =
  extract_test
    {|
let z = 0
let f () =
  $z + 1$
|};
  [%expect
    {|
    let z = 0
    let fun_name1 () = z + 1
    let f () =
      fun_name1 ()
    |}]
;;
