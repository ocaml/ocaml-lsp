open Test.Import

let print_hover_extended resp =
  resp |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover_extended client position verbosity =
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
  Test.custom_request client "ocamllsp/hoverExtended" (`Assoc params)
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
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
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
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
;;

let%expect_test "hover returns type inferred under cursor in a formatted way" =
  let source =
    {ocaml|
let f a b c d e f g h i = 1 + a + b + c + d + e + f + g + h + i
|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "int ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint"
      },
      "range": {
        "end": { "character": 5, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    } |}]
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
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    Fiber.return ()
  in
  Helpers.test ~extra_env:[ "OCAMLLSP_HOVER_IS_EXTENDED=true" ] source req;
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
    } |}]
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
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
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
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
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
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
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
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
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
  Helpers.test source req;
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
    {
      "contents": { "kind": "plaintext", "value": "int option" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 4, "line": 3 }
      }
    } |}]
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
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "plaintext",
        "value": "int ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint ->\nint"
      },
      "range": {
        "end": { "character": 5, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    } |}]
;;

let%expect_test "hoverExtended returns type inferred under cursor" =
  let source =
    {ocaml|let x = 1
|ocaml}
  in
  let position = Position.create ~line:0 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int" },
      "range": {
        "end": { "character": 5, "line": 0 },
        "start": { "character": 4, "line": 0 }
      }
    }
    |}]
;;

let%expect_test "hoverExtended returns type inferred under cursor with markdown" =
  let source =
    {ocaml|let x = 1
|ocaml}
  in
  let position = Position.create ~line:0 ~character:4 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test ~capabilities:Hover_helpers.markdown_capabilities source req;
  [%expect
    {|
    {
      "contents": { "kind": "markdown", "value": "```ocaml\nint\n```" },
      "range": {
        "end": { "character": 5, "line": 0 },
        "start": { "character": 4, "line": 0 }
      }
    }
    |}]
;;

let%expect_test "hoverExtended returns type inferred under cursor with documentation" =
  let source = Hover_helpers.documented_id_use_source in
  let position = Position.create ~line:3 ~character:9 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test ~capabilities:Hover_helpers.markdown_capabilities source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\n'a -> 'a\n```\n***\nThis function has a nice documentation"
      },
      "range": {
        "end": { "character": 11, "line": 3 },
        "start": { "character": 9, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "hoverExtended returns type inferred under cursor with documentation tags"
  =
  let source = Hover_helpers.documented_div_use_source in
  let position = Position.create ~line:23 ~character:10 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test ~capabilities:Hover_helpers.markdown_capabilities source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\nint -> int -> int\n```\n***\nThis function has a nice documentation.\n\nIt performs division of two integer numbers.\n\n***@param*** `x`\ndividend\n\n***@param*** divisor\n\n***@return***\n*quotient*, i.e. result of division\n\n***@raise*** `Division_by_zero`\nraised when divided by zero\n\n***@see*** [link](https://en.wikipedia.org/wiki/Arithmetic#Division_\\(%C3%B7,_or_/\\))\narticle\n\n***@see*** `arithmetic.ml`\nfor more context\n\n***@since*** `4.0.0`\n\n***@before*** `4.4.0`\n\n***@deprecated***\nuse `(/)`\n\n***@version*** `1.0.0`\n\n***@author*** John Doe"
      },
      "range": {
        "end": { "character": 11, "line": 23 },
        "start": { "character": 8, "line": 23 }
      }
    }
    |}]
;;

let%expect_test "hoverExtended returns good type when cursor is between values" =
  let source =
    {ocaml|let f i f = float_of_int i +. f
let i = 10
let f = 10.
let sum = f i f
|ocaml}
  in
  let position = Position.create ~line:3 ~character:13 in
  let req client =
    let* resp = hover_extended client position (Some 0) in
    let () = print_hover_extended resp in
    Fiber.return ()
  in
  Helpers.test ~capabilities:Hover_helpers.markdown_capabilities source req;
  [%expect
    {|
    {
      "contents": { "kind": "markdown", "value": "```ocaml\nint\n```" },
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 12, "line": 3 }
      }
    }
    |}]
;;

let%expect_test "hoverExtended regression test for #343" =
  let source =
    {ocaml|type t = s
and s = string
type 'a fib = ('a -> unit) -> unit
|ocaml}
  in
  let req client =
    let* hover1 = hover_extended client (Position.create ~line:1 ~character:4) (Some 0) in
    print_hover_extended hover1;
    let* hover2 = hover_extended client (Position.create ~line:2 ~character:9) (Some 0) in
    print_hover_extended hover2;
    Fiber.return ()
  in
  Helpers.test ~capabilities:Hover_helpers.markdown_capabilities source req;
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\ntype s = string\n```"
      },
      "range": {
        "end": { "character": 14, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    }
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\ntype 'a fib = ('a -> unit) -> unit\n```"
      },
      "range": {
        "end": { "character": 34, "line": 2 },
        "start": { "character": 0, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "hoverExtended regression test for #403" =
  let source =
    {ocaml|type foo = int

let x : foo = 1
|ocaml}
  in
  let req client =
    let* resp = hover_extended client (Position.create ~line:2 ~character:4) (Some 0) in
    print_hover_extended resp;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "foo" },
      "range": {
        "end": { "character": 5, "line": 2 },
        "start": { "character": 4, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "hoverExtended regression test for #344" =
  let source = Stdlib.String.make 24 '\n' ^ "let k = ()\nlet m = List.map\n" in
  let request_hover_over_k client =
    hover_extended client (Position.create ~line:24 ~character:4) (Some 0)
  in
  let req client =
    let* hover_over_k = request_hover_over_k client in
    print_hover_extended hover_over_k;
    let* (_ : Yojson.Safe.t) =
      hover_extended client (Position.create ~line:25 ~character:15) (Some 0)
    in
    let* hover_over_k = request_hover_over_k client in
    print_hover_extended hover_over_k;
    Fiber.return ()
  in
  Helpers.test ~capabilities:Hover_helpers.markdown_capabilities source req;
  [%expect
    {|
    {
      "contents": { "kind": "markdown", "value": "```ocaml\nunit\n```" },
      "range": {
        "end": { "character": 5, "line": 24 },
        "start": { "character": 4, "line": 24 }
      }
    }
    {
      "contents": { "kind": "markdown", "value": "```ocaml\nunit\n```" },
      "range": {
        "end": { "character": 5, "line": 24 },
        "start": { "character": 4, "line": 24 }
      }
    }
    |}]
;;
