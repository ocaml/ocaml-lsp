open Async
open Test.Import

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let print_hover_extended resp =
  resp |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover_extended client position verbosity =
  let params =
    let required =
      [ ( "textDocument"
        , TextDocumentIdentifier.yojson_of_t
            (TextDocumentIdentifier.create ~uri:Helpers.uri) )
      ; "position", Position.yojson_of_t position
      ]
    in
    let params =
      match verbosity with
      | None -> required
      | Some v -> ("verbosity", `Int v) :: required
    in
    Some (Jsonrpc.Structured.t_of_yojson (`Assoc params))
  in
  Client.request client (UnknownRequest { meth = "ocamllsp/hoverExtended"; params })
;;

let%expect_test "hover reference" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  (* This test shows the default hover behavior, and we enable extendedHover by default.
     Testing the disabling of extendedHover requires threading through extra logic and
     isn't worth the effort (setting OCAMLLSP_HOVER_IS_EXTENDED=false just uses the
     default value, which is [true]). *)
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "hover returns type inferred under cursor in a formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "int -> int -> int -> int -> int -> int -> int -> int -> int -> int\n***\nAllocation: heap"
      },
      "range": {
        "end": { "character": 5, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "hover extended" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  let%map.Deferred () =
    Helpers.test ~extra_env:[ "OCAMLLSP_HOVER_IS_EXTENDED=true" ] source req
  in
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "default verbosity" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "explicit verbosity 0" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "explicit verbosity 1" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 1) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 1,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": true,
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "explicit verbosity 2" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 2) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 2,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": true,
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "implicity verbosity increases" =
  let source =
    {ocaml|
type foo = int option

let foo_value : foo = Some 1
|ocaml}
  in
  let position = Position.create ~line:3 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": true,
      "canDecreaseVerbosity": false,
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    {
      "verbosity": 1,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": true,
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    {
      "verbosity": 2,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": true,
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "hover extended returns type inferred under cursor in a formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "int -> int -> int -> int -> int -> int -> int -> int -> int -> int\n***\nAllocation: heap"
      },
      "range": {
        "end": { "character": 5, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "heap allocation" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  Some (g z)
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:3 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'a option\n***\nSome\n***\nAllocation: heap"
      },
      "range": {
        "end": { "character": 6, "line": 3 },
        "start": { "character": 2, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "stack allocation" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  exclave_ Some (g z)
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:12 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'a option\n***\nSome\n***\nAllocation: stack"
      },
      "range": {
        "end": { "character": 15, "line": 3 },
        "start": { "character": 11, "line": 3 }
      }
    }
    |}]
;;

(* NOTE: This only seems to work on the character to the right of the +

   This looks like a bug in the stack-or-heap-enclosing query where it returns the
   range 2:11 - 2:12 when called on the +, instead of 2:12 - 2:13 like it should.

   Merlin devs will look into fixing this.
*)
let%expect_test "no relevant allocation to show" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  Some (g z)
;;
|ocaml}
  in
  let position = Position.create ~line:2 ~character:13 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "int -> int -> int\n***\nInteger addition.\n    Left-associative operator, see {!Ocaml_operators} for more information.\n***\nAllocation: no relevant allocation to show"
      },
      "range": {
        "end": { "character": 13, "line": 2 },
        "start": { "character": 12, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "not an allocation (constructor without arguments)" =
  let source =
    {ocaml|
let f g x y =
  let z = x + y in
  None
;;
|ocaml}
  in
  let position = Position.create ~line:3 ~character:2 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a option\n***\nNone\n***\nAllocation: not an allocation (constructor without arguments)"
      },
      "range": {
        "end": { "character": 6, "line": 3 },
        "start": { "character": 2, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "could be stack or heap" =
  let source =
    {ocaml|
let f g x y =
  let z = Some (g z) in
  y
;;
|ocaml}
  in
  let position = Position.create ~line:2 ~character:10 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'a option\n***\nSome\n***\nAllocation: could be stack or heap"
      },
      "range": {
        "end": { "character": 14, "line": 2 },
        "start": { "character": 10, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "function on the stack" =
  let source =
    {ocaml|
let f g =
  exclave_ fun x -> g x
;;
|ocaml}
  in
  let position = Position.create ~line:2 ~character:11 in
  let req client =
    let* resp = hover_extended client position None in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  let%map.Deferred () = Helpers.test source req in
  [%expect
    {|
    {
      "verbosity": 0,
      "canIncreaseVerbosity": false,
      "canDecreaseVerbosity": false,
      "contents": {
        "kind": "plaintext",
        "value": "'a -> 'b\n***\nAllocation: stack"
      },
      "range": {
        "end": { "character": 23, "line": 2 },
        "start": { "character": 11, "line": 2 }
      }
    }
    |}]
;;
