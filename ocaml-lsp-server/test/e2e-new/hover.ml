open Test.Import

let%expect_test "returns type inferred under cursor" =
  let source =
    {ocaml|let x = 1
|ocaml}
  in
  let position = Position.create ~line:0 ~character:4 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
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

let%expect_test "uses UTF-16 positions around astral Unicode characters" =
  let source = "let s = \"😀\";; let x = 1;; x\n" in
  (* The final [x] starts at UTF-16 code unit 27. Its UTF-8 byte offset is 29. *)
  let position = Position.create ~line:0 ~character:27 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int" },
      "range": {
        "end": { "character": 28, "line": 0 },
        "start": { "character": 27, "line": 0 }
      }
    }
    |}]
;;

let%expect_test "returns type inferred under cursor (markdown formatting)" =
  let source =
    {ocaml|let x = 1
|ocaml}
  in
  let position = Position.create ~line:0 ~character:4 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
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

let%expect_test "returns type inferred under cursor with documentation in let-definition" =
  let source =
    {ocaml|(** This function has a nice documentation *)
let id x = x

|ocaml}
  in
  let position = Position.create ~line:1 ~character:4 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
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
        "end": { "character": 6, "line": 1 },
        "start": { "character": 4, "line": 1 }
      }
    }
    |}]
;;

let%expect_test "returns type inferred under cursor with documentation" =
  let source = Hover_helpers.documented_id_use_source in
  let position = Position.create ~line:3 ~character:9 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
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

let%expect_test
    "returns type inferred under cursor with documentation with tags (markdown \
     formatting)"
  =
  let source = Hover_helpers.documented_div_use_source in
  let position = Position.create ~line:23 ~character:9 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
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

let%expect_test "returns good type when cursor is between values" =
  let source =
    {ocaml|let f i f = float_of_int i +. f
let i = 10
let f = 10.
let sum = f i f
|ocaml}
  in
  let position = Position.create ~line:3 ~character:13 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
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

let%expect_test "regression test for #343" =
  let source =
    {ocaml|type t = s
and s = string
type 'a fib = ('a -> unit) -> unit
|ocaml}
  in
  let req client =
    let* hover1 = Hover_helpers.hover client (Position.create ~line:1 ~character:4) in
    Hover_helpers.print_hover hover1;
    let* hover2 = Hover_helpers.hover client (Position.create ~line:2 ~character:9) in
    Hover_helpers.print_hover hover2;
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

let%expect_test "regression test for #403" =
  let source =
    {ocaml|type foo = int

let x : foo = 1
|ocaml}
  in
  let position = Position.create ~line:2 ~character:4 in
  let req client =
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
        "end": { "character": 5, "line": 2 },
        "start": { "character": 4, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "FIXME: reproduce [#344](https://github.com/ocaml/ocaml-lsp/issues/344)" =
  let source = Stdlib.String.make 24 '\n' ^ "let k = ()\nlet m = List.map\n" in
  let request_hover_over_k client =
    Hover_helpers.hover client (Position.create ~line:24 ~character:4)
  in
  let req client =
    let* hover_over_k = request_hover_over_k client in
    Hover_helpers.print_hover hover_over_k;
    let* (_ : Hover.t option) =
      Hover_helpers.hover client (Position.create ~line:25 ~character:15)
    in
    let* hover_over_k = request_hover_over_k client in
    Hover_helpers.print_hover hover_over_k;
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

let%expect_test "object method call" =
  let source =
    {ocaml|
let f (o : <  g : int -> unit >) = o#g 4
|ocaml}
  in
  let position = Position.create ~line:1 ~character:38 in
  let req client =
    let* resp = Hover_helpers.hover client position in
    let () = Hover_helpers.print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int -> unit" },
      "range": {
        "end": { "character": 38, "line": 1 },
        "start": { "character": 35, "line": 1 }
      }
    }
    |}]
;;
