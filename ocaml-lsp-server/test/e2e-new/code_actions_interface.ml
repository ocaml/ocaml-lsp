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
  let intf_source = "" in
  let range = range ~start_line:0 ~start_character:0 ~end_line:0 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "inferred_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "type t = Foo of int | Bar of bool\n\nval f : t -> t\n",
                "range": {
                  "end": { "character": 0, "line": 0 },
                  "start": { "character": 0, "line": 0 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "inferred_intf",
      "title": "Insert inferred interface"
    } |}]
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
  let intf_source =
    {ocaml|
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
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "type t = Foo of int | Bar of bool\n",
                "range": {
                  "end": { "character": 0, "line": 0 },
                  "start": { "character": 0, "line": 0 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "inferred_intf",
      "title": "Insert inferred interface"
    }
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
  let intf_source =
    {ocaml|
type t = Foo of int | Bar of bool
val f : t -> bool
|ocaml}
  in
  let range = range ~start_line:2 ~start_character:0 ~end_line:2 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "val f : t -> bool -> bool\n",
                "range": {
                  "end": { "character": 17, "line": 2 },
                  "start": { "character": 0, "line": 2 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
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
  let intf_source =
    {ocaml|
val f : int -> string -> 'a list -> bool -> bool
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:10 ~end_line:1 ~end_character:10 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
  Code actions:
  {
    "edit": {
      "documentChanges": [
        {
          "edits": [
            {
              "newText": "val f : int -> string -> bool -> bool\n",
              "range": {
                "end": { "character": 48, "line": 1 },
                "start": { "character": 0, "line": 1 }
              }
            }
          ],
          "textDocument": { "uri": "file:///foo.mli", "version": 0 }
        }
      ]
    },
    "isPreferred": false,
    "kind": "update_intf",
    "title": "Update signature(s) to match implementation"
  }
  |}]
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
  let intf_source =
    {ocaml|
val f : int -> string -> 'a list -> bool -> bool
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:1 ~end_line:1 ~end_character:12 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "val f : int -> 'a list -> 'b list -> bool -> bool\n",
                "range": {
                  "end": { "character": 48, "line": 1 },
                  "start": { "character": 0, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
    |}]
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
  let intf_source =
    {ocaml|
val f :
    int  (* This comment should stay. *)
    -> int

val g : int
    -> int (* This comment should disappear since the function changes. *)
    -> int

(* This comment should stay even though the function changes. *)
val h : int -> bool
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:0 ~end_line:10 ~end_character:19 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "val g :\n  int ->\n  int ->\n  int ->\n  another_arg:int -> yet_another_arg:int -> keep_them_coming:int -> int\n",
                "range": {
                  "end": { "character": 10, "line": 7 },
                  "start": { "character": 0, "line": 5 }
                }
              },
              {
                "newText": "val h : float -> float\n",
                "range": {
                  "end": { "character": 19, "line": 10 },
                  "start": { "character": 0, "line": 10 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
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
  let intf_source =
    {ocaml|
module M : sig type t = I of int | B of bool end
|ocaml}
  in
  let range = range ~start_line:1 ~start_character:0 ~end_line:1 ~end_character:0 in
  print_code_actions
    intf_source
    range
    ~prep
    ~path:"foo.mli"
    ~filter:(find_action "update_intf");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "module M :\n  sig\n    type t = I of int | F of float\n    val f : t -> long_name_for_an_integer_argument:int -> int\n  end\n",
                "range": {
                  "end": { "character": 48, "line": 1 },
                  "start": { "character": 0, "line": 1 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.mli", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "update_intf",
      "title": "Update signature(s) to match implementation"
    }
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
  let source =
    {ocaml|
    match card with
    | Ace -> _
    | King -> _
    | Queen -> "Face card!"
    | Jack -> "Face card?"
    | Number _ -> _
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:3 ~end_line:6 ~end_character:6 in
  print_code_actions source range ~filter:(find_action "combine-cases");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "    | King | Queen | Jack | Number _ -> _\n",
                "range": {
                  "end": { "character": 0, "line": 7 },
                  "start": { "character": 0, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "combine-cases",
      "title": "Combine-cases"
    }
    |}]
;;

let%expect_test "can combine cases with one unique RHS" =
  let source =
    {ocaml|
    match card with
    | Ace -> _
    | King -> _
    | Queen -> "Face card!"
    | Jack -> "Face card?"
    | Number _ -> _
|ocaml}
  in
  let range = range ~start_line:3 ~start_character:3 ~end_line:4 ~end_character:4 in
  print_code_actions source range ~filter:(find_action "combine-cases");
  [%expect
    {|
    Code actions:
    {
      "edit": {
        "documentChanges": [
          {
            "edits": [
              {
                "newText": "    | King | Queen -> \"Face card!\"\n",
                "range": {
                  "end": { "character": 0, "line": 5 },
                  "start": { "character": 0, "line": 3 }
                }
              }
            ],
            "textDocument": { "uri": "file:///foo.ml", "version": 0 }
          }
        ]
      },
      "isPreferred": false,
      "kind": "combine-cases",
      "title": "Combine-cases"
    }
    |}]
;;
