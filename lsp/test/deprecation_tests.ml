module Deprecation = Lsp.Deprecation

let string_of_option f = function
  | None -> "none"
  | Some x -> f x
;;

let print { Deprecation.deprecated; tags } =
  let deprecated = string_of_option string_of_bool deprecated in
  let tags = string_of_option (String.concat ",") tags in
  Printf.printf "deprecated=%s tags=%s\n" deprecated tags
;;

let%expect_test "deprecation representation" =
  Deprecation.create
    ~deprecated:false
    ~tag:"deprecated"
    ~supports_tag:true
    ~supports_deprecated_field:true
  |> print;
  [%expect {| deprecated=none tags=none |}];
  Deprecation.create
    ~deprecated:true
    ~tag:"deprecated"
    ~supports_tag:true
    ~supports_deprecated_field:true
  |> print;
  [%expect {| deprecated=none tags=deprecated |}];
  Deprecation.create
    ~deprecated:true
    ~tag:"deprecated"
    ~supports_tag:false
    ~supports_deprecated_field:true
  |> print;
  [%expect {| deprecated=true tags=none |}];
  Deprecation.create
    ~deprecated:true
    ~tag:"deprecated"
    ~supports_tag:false
    ~supports_deprecated_field:false
  |> print;
  [%expect {| deprecated=none tags=none |}]
;;

let%expect_test "tag membership" =
  let test tags ~tag =
    Deprecation.tag_supported tags ~tag ~equal:String.equal
    |> string_of_bool
    |> print_endline
  in
  test [ "a"; "b" ] ~tag:"b";
  [%expect {| true |}];
  test [ "a"; "b" ] ~tag:"c";
  [%expect {| false |}]
;;
