let extract_local_test = Code_actions.code_action_test ~title:"Extract local"
let extract_function_test = Code_actions.code_action_test ~title:"Extract function"

let%expect_test "extract local constant" =
  extract_local_test {|
let f =
  0 + $1$
|};
  [%expect {|
    let f =
      let var_name = 1 in
    0 + var_name |}]
;;

let%expect_test "extract local expression" =
  extract_local_test {|
let f =
  let x = 2 in
  $0 + 1 + x$ + 1
|};
  [%expect
    {|
    let f =
      let x = 2 in
      let var_name = 0 + 1 + x in
    var_name + 1 |}]
;;

let%expect_test "extract function single parameter" =
  extract_function_test {|
let f x =
  $(x * 2)$ + 3
|};
  [%expect {|
    let fun_name x = (x * 2)

    let f x =
      fun_name x + 3 |}]
;;

let%expect_test "extract function multiple parameter" =
  extract_function_test {|
let f x =
  let y = 0 in
  $(x * y)$ + 3
|};
  [%expect
    {|
    let fun_name y x = (x * y)

    let f x =
      let y = 0 in
      fun_name y x + 3 |}]
;;

let%expect_test "extract function with local module" =
  extract_function_test
    {|
let f x =
  let module M = struct
    let y = 0
  end in
  $(x * M.y)$ + 3
|};
  [%expect {||}]
;;

(* TODO: This extraction shouldn't be allowed. *)
let%expect_test "extract function with local exception" =
  extract_function_test {|
let f x =
  let exception Local in
  $raise Local$
|};
  [%expect
    {|
    let fun_name () = raise Local

    let f x =
      let exception Local in
      fun_name () |}]
;;

let%expect_test "extract function with shadowed parameter" =
  extract_function_test {|
let x = 0
let f x = $x + 1$
|};
  [%expect {|
    let x = 0
    let fun_name x = x + 1

    let f x = fun_name x |}]
;;

let%expect_test "extract function with bound variable" =
  extract_function_test {|
let x = 0
let y = 1
let f x = $x + y$
|};
  [%expect
    {|
    let x = 0
    let y = 1
    let fun_name x = x + y

    let f x = fun_name x |}]
;;

let%expect_test "extract higher order function" =
  extract_function_test {|
let f x =
  $List.map (fun y -> y + 1) x$
|};
  [%expect
    {|
    let fun_name x = List.map (fun y -> y + 1) x

    let f x =
      fun_name x |}]
;;

let%expect_test "extract higher order function" =
  extract_function_test {|
let f y =
  $List.map (fun y -> y + 1) y$
|};
  [%expect
    {|
    let fun_name y = List.map (fun y -> y + 1) y

    let f y =
      fun_name y |}]
;;

let%expect_test "extract higher order function" =
  extract_function_test {|
let f y =
  $List.map (fun y -> y + 1) y$
|};
  [%expect
    {|
    let fun_name y = List.map (fun y -> y + 1) y

    let f y =
      fun_name y |}]
;;

let%expect_test "extract inside let binding" =
  extract_function_test {|
let f y =
  let y = y + 1 in
  $y + 2$
|};
  [%expect
    {|
    let fun_name y = y + 2

    let f y =
      let y = y + 1 in
      fun_name y |}]
;;

let%expect_test "extract free variable" =
  extract_function_test {|
let f () =
  $z + 1$
|};
  [%expect {|
    let fun_name () = z + 1

    let f () =
      fun_name () |}]
;;
