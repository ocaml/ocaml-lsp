open Test.Import
open Lsp_helpers
open Code_actions

let%expect_test "can infer module interfaces" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  code_action_test
    ~prep
    ~path:"foo.mli"
    ~print_none:true
    ~title:"Insert inferred interface"
    {ocaml|$|ocaml};
  [%expect
    {|
    type t = Foo of int | Bar of bool

    val f : t -> t
    |}]
;;

let%expect_test "inferred interface excludes existing names" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  code_action_test
    ~prep
    ~path:"foo.mli"
    ~print_none:true
    ~title:"Insert inferred interface"
    {ocaml|$
val f : t -> t
|ocaml};
  [%expect
    {|
    type t = Foo of int | Bar of bool

    val f : t -> t
    |}]
;;

let%expect_test "no inferred interface when the interface is complete" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) = x
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  let intf_source =
    {ocaml|
type t = Foo of int | Bar of bool
val f : t -> t
|ocaml}
  in
  let range = range ~start_line:0 ~start_character:0 ~end_line:0 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "inferred_intf");
  [%expect {| No code actions |}]
;;

let%expect_test "update-signatures adds new function args" =
  let impl_source =
    {ocaml|
type t = Foo of int | Bar of bool
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  code_action_test
    ~prep
    ~path:"foo.mli"
    ~print_none:true
    ~title:"Update signature(s) to match implementation"
    {ocaml|
type t = Foo of int | Bar of bool
$val f : t -> bool
|ocaml};
  [%expect
    {|
    type t = Foo of int | Bar of bool
    val f : t -> bool -> bool
    |}]
;;

let%expect_test "update-signatures removes old function args" =
  let impl_source =
    {ocaml|
let f i s b =
  if b then String.length s > i else String.length s < i
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  code_action_test
    ~prep
    ~path:"foo.mli"
    ~print_none:true
    ~title:"Update signature(s) to match implementation"
    {ocaml|
val f : in$t -> string -> 'a list -> bool -> bool
|ocaml};
  [%expect {| val f : int -> string -> bool -> bool |}]
;;

let%expect_test "update-signatures updates parameter types" =
  let impl_source =
    {ocaml|
let f i s l b =
  if b then List.length s > i else List.length l < i
  |ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  code_action_test
    ~prep
    ~path:"foo.mli"
    ~print_none:true
    ~title:"Update signature(s) to match implementation"
    {ocaml|
v$al f : int $-> string -> 'a list -> bool -> bool
|ocaml};
  [%expect {| val f : int -> 'a list -> 'b list -> bool -> bool |}]
;;

let%expect_test "update-signatures preserves functions and their comments" =
  let impl_source =
    {ocaml|
let f x = x + 1;;

let g x y z ~another_arg ~yet_another_arg ~keep_them_coming = x - y + z + another_arg + yet_another_arg + keep_them_coming;;

let h x = x *. 2.0;;
  |ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  code_action_test
    ~prep
    ~path:"foo.mli"
    ~print_none:true
    ~title:"Update signature(s) to match implementation"
    {ocaml|
$val f :
    int  (* This comment should stay. *)
    -> int

val g : int
    -> int (* This comment should disappear since the function changes. *)
    -> int

(* This comment should stay even though the function changes. *)
val h : int -> bool$
|ocaml};
  [%expect
    {|
    val f :
        int  (* This comment should stay. *)
        -> int

    val g :
      int ->
      int ->
      int ->
      another_arg:int -> yet_another_arg:int -> keep_them_coming:int -> int


    (* This comment should stay even though the function changes. *)
    val h : float -> float
    |}]
;;

let%expect_test "update-signatures updates modules" =
  let impl_source =
    {ocaml|
module M = struct
  type t =
    | I of int
    | F of float
  ;;
  let f (x : t) ~long_name_for_an_integer_argument =
    match x with
    | I i -> i
    | F f -> long_name_for_an_integer_argument
  ;;
end
|ocaml}
  in
  let uri = DocumentUri.of_path "foo.ml" in
  let prep client = Test.open_document ~client ~uri ~source:impl_source () in
  code_action_test
    ~prep
    ~path:"foo.mli"
    ~print_none:true
    ~title:"Update signature(s) to match implementation"
    {ocaml|
$module M : sig type t = I of int | B of bool end
|ocaml};
  [%expect
    {|
    module M :
      sig
        type t = I of int | F of float
        val f : t -> long_name_for_an_integer_argument:int -> int
      end
    |}]
;;

let activate_jump client =
  let config =
    DidChangeConfigurationParams.create
      ~settings:(`Assoc [ "merlinJumpCodeActions", `Assoc [ "enable", `Bool true ] ])
  in
  change_config ~client config
;;

let%expect_test "can jump to match target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-match");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 2, "line": 4 },
            "start": { "character": 2, "line": 4 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Match jump"
      },
      "kind": "merlin-jump-match",
      "title": "Match jump"
    }

       |}]
;;

let%expect_test "can jump to match-next-case target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-next-case");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 3, "line": 6 },
            "start": { "character": 3, "line": 6 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Next-case jump"
      },
      "kind": "merlin-jump-next-case",
      "title": "Next-case jump"
    } |}]
;;

let%expect_test "can jump to  match-prev-case target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-prev-case");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 3, "line": 5 },
            "start": { "character": 3, "line": 5 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Prev-case jump"
      },
      "kind": "merlin-jump-prev-case",
      "title": "Prev-case jump"
    } |}]
;;

let%expect_test "can jump to let target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-let");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 0, "line": 3 },
            "start": { "character": 0, "line": 3 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Let jump"
      },
      "kind": "merlin-jump-let",
      "title": "Let jump"
    } |}]
;;

let%expect_test "can jump to fun target" =
  let source =
    {ocaml|
type t = Foo of int | Bar of bool
let square x = x * x
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:5 ~start_character:5 ~end_line:5 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-fun");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 0, "line": 3 },
            "start": { "character": 0, "line": 3 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Fun jump"
      },
      "kind": "merlin-jump-fun",
      "title": "Fun jump"
    } |}]
;;

let%expect_test "can jump to module target" =
  let source =
    {ocaml|
module FooBar = struct
  type t = Foo of int | Bar of bool
end
let f (x : t) (d : bool) =
  match x with
  |Bar x -> x
  |Foo _ -> d
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:5 ~end_line:2 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-module");
  [%expect
    {|
    Code actions:
    {
      "command": {
        "arguments": [
          "file:///foo.ml",
          {
            "end": { "character": 0, "line": 1 },
            "start": { "character": 0, "line": 1 }
          }
        ],
        "command": "ocamllsp/merlin-jump-to-target",
        "title": "Module jump"
      },
      "kind": "merlin-jump-module",
      "title": "Module jump"
    } |}]
;;

let%expect_test "can jump to module-type target" =
  let source =
    {ocaml|
  module type ORDER = sig
    type t
    val leq : t -> t -> bool
    val equal : t -> t -> bool
  end

  let f (x : t) (d : bool) =
    match x with
    |Bar x -> x
    |Foo _ -> d
  |ocaml}
  in
  let range = range ~start_line:4 ~start_character:5 ~end_line:4 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-module-type");
  [%expect
    {|
      Code actions:
      {
        "command": {
          "arguments": [
            "file:///foo.ml",
            {
              "end": { "character": 2, "line": 1 },
              "start": { "character": 2, "line": 1 }
            }
          ],
          "command": "ocamllsp/merlin-jump-to-target",
          "title": "Module-type jump"
        },
        "kind": "merlin-jump-module-type",
        "title": "Module-type jump"
      } |}]
;;

let%expect_test "shouldn't find the jump target on the same line" =
  let source =
    {ocaml|
  let square x = x * x
  let f (x : t) (d : bool) =
    match x with
    |Bar x -> x
    |Foo _ -> d
  |ocaml}
  in
  let range = range ~start_line:0 ~start_character:5 ~end_line:0 ~end_character:5 in
  print_code_actions
    ~prep:activate_jump
    source
    range
    ~filter:(find_action "merlin-jump-fun");
  [%expect
    {|
      No code actions |}]
;;

let%expect_test "can combine cases with multiple RHSes" =
  code_action_test
    ~print_none:true
    ~title:"Combine-cases"
    {ocaml|
    match card with
    | Ace -> _
   $ | King -> _
    | Queen -> "Face card!"
    | Jack -> "Face card?"
    | $Number _ -> _
|ocaml};
  [%expect
    {|
    match card with
    | Ace -> _
    | King | Queen | Jack | Number _ -> _
    |}]
;;

let%expect_test "can combine cases with one unique RHS" =
  code_action_test
    ~print_none:true
    ~title:"Combine-cases"
    {ocaml|
    match card with
    | Ace -> _
   $ | King -> _
    $| Queen -> "Face card!"
    | Jack -> "Face card?"
    | Number _ -> _
|ocaml};
  [%expect
    {|
    match card with
    | Ace -> _
    | King | Queen -> "Face card!"
    | Jack -> "Face card?"
    | Number _ -> _
    |}]
;;
