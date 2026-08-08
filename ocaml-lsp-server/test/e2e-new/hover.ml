open Test.Import

let%expect_test "returns type inferred under cursor" =
  let source =
    {ocaml|let x = 1
|ocaml}
  in
  Hover_helpers.test_hover source [ Position.create ~line:0 ~character:4 ];
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
  Hover_helpers.test_hover source [ Position.create ~line:0 ~character:27 ];
  [%expect {| no hover response |}]
;;

let%expect_test "returns type inferred under cursor (markdown formatting)" =
  let source =
    {ocaml|let x = 1
|ocaml}
  in
  Hover_helpers.test_hover
    ~capabilities:Hover_helpers.markdown_capabilities
    source
    [ Position.create ~line:0 ~character:4 ];
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
  Hover_helpers.test_hover
    ~capabilities:Hover_helpers.markdown_capabilities
    source
    [ Position.create ~line:1 ~character:4 ];
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
  Hover_helpers.test_hover
    ~capabilities:Hover_helpers.markdown_capabilities
    source
    [ Position.create ~line:3 ~character:9 ];
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

let%expect_test "renders extended documentation markup in hover" =
  let source =
    {ocaml|(** {%html:<span>raw</span>%}

    Inline math: {m x + y}

    {math z = x + y}

    {!modules:List Array}

    @param value input
    @return output
    @canonical Package.Module
    @version 2.0
*)
let documented value = value
|ocaml}
  in
  Hover_helpers.test_hover
    ~capabilities:Hover_helpers.markdown_capabilities
    source
    [ Position.create ~line:13 ~character:5 ];
  [%expect
    {|
    {
      "contents": {
        "kind": "markdown",
        "value": "```ocaml\n'a -> 'a\n```\n***\n<span>raw</span>\n\nInline math: $x + y$\n\n```\nz = x + y\n```\n\n* List\n* Array\n\n***@param*** `value`\ninput\n\n***@return***\noutput\n\n***@canonical*** Package.Module\n\n***@version*** `2.0`"
      },
      "range": {
        "end": { "character": 14, "line": 13 },
        "start": { "character": 4, "line": 13 }
      }
    }
    |}]
;;

let%expect_test
    "returns type inferred under cursor with documentation with tags (markdown \
     formatting)"
  =
  let source = Hover_helpers.documented_div_use_source in
  Hover_helpers.test_hover
    ~capabilities:Hover_helpers.markdown_capabilities
    source
    [ Position.create ~line:23 ~character:9 ];
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
  Hover_helpers.test_hover
    ~capabilities:Hover_helpers.markdown_capabilities
    source
    [ Position.create ~line:3 ~character:13 ];
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

let%expect_test "returned range does not contain the hover position" =
  let source = "let rec f x = x :: f x" in
  Hover_helpers.test_hover source [ Position.create ~line:0 ~character:18 ];
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "'a list" },
      "range": {
        "end": { "character": 22, "line": 0 },
        "start": { "character": 19, "line": 0 }
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
  Hover_helpers.test_hover
    ~capabilities:Hover_helpers.markdown_capabilities
    source
    [ Position.create ~line:1 ~character:4; Position.create ~line:2 ~character:9 ];
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
  Hover_helpers.test_hover source [ Position.create ~line:2 ~character:4 ];
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

let%expect_test "empty odoc table raises an internal error" =
  let source =
    {ocaml|(** {table} *)
let x = 1
let _ = x
|ocaml}
  in
  let request client =
    let position = Position.create ~line:2 ~character:8 in
    let* result = Fiber.collect_errors (fun () -> Hover_helpers.hover client position) in
    match result with
    | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
      ->
      let data = Option.value_exn error.data in
      let exn = Yojson.Safe.Util.(data |> member "exn" |> to_string) in
      Printf.printf
        "code: %s\nexception: %s\n"
        (Jsonrpc.Response.Error.Code.to_string error.code)
        exn;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors
    | Ok hover ->
      Hover_helpers.print_hover hover;
      Fiber.return ()
  in
  Helpers.test ~capabilities:Hover_helpers.markdown_capabilities source request;
  [%expect
    {|
    {
      "contents": { "kind": "markdown", "value": "```ocaml\nint\n```\n***\n" },
      "range": {
        "end": { "character": 9, "line": 2 },
        "start": { "character": 8, "line": 2 }
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
  Hover_helpers.test_hover source [ Position.create ~line:1 ~character:38 ];
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

let%expect_test "hover on constructor and exception declarations" =
  let source =
    {ocaml|exception Exn of int
type t = A of string
let use = Exn 1
|ocaml}
  in
  Hover_helpers.test_hover
    source
    [ Position.create ~line:0 ~character:11
    ; Position.create ~line:1 ~character:10
    ; Position.create ~line:2 ~character:11
    ];
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "int -> exn" },
      "range": {
        "end": { "character": 13, "line": 0 },
        "start": { "character": 10, "line": 0 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "string -> t" },
      "range": {
        "end": { "character": 10, "line": 1 },
        "start": { "character": 9, "line": 1 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "int -> exn" },
      "range": {
        "end": { "character": 13, "line": 2 },
        "start": { "character": 10, "line": 2 }
      }
    }
    |}]
;;

let%expect_test "hover on module, class and signature items" =
  let source =
    {ocaml|
module type S = sig
  module N : sig end
  open S
end
module M : S = struct end
class type ct = object method m : int end
type point = { px : int; py : int }
let f ({ px; py } as p : point) = px + py
|ocaml}
  in
  Hover_helpers.test_hover
    source
    [ Position.create ~line:2 ~character:9
    ; Position.create ~line:3 ~character:7
    ; Position.create ~line:5 ~character:7
    ; Position.create ~line:6 ~character:11
    ; Position.create ~line:8 ~character:9
    ; Position.create ~line:8 ~character:11
    ];
  [%expect
    {|
    {
      "contents": { "kind": "plaintext", "value": "sig end" },
      "range": {
        "end": { "character": 10, "line": 2 },
        "start": { "character": 9, "line": 2 }
      }
    }
    {
      "contents": {
        "kind": "plaintext",
        "value": "sig\n  module N : sig end\nend"
      },
      "range": {
        "end": { "character": 3, "line": 4 },
        "start": { "character": 16, "line": 1 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "sig end" },
      "range": {
        "end": { "character": 8, "line": 5 },
        "start": { "character": 7, "line": 5 }
      }
    }
    no hover response
    {
      "contents": { "kind": "plaintext", "value": "int" },
      "range": {
        "end": { "character": 11, "line": 8 },
        "start": { "character": 9, "line": 8 }
      }
    }
    {
      "contents": { "kind": "plaintext", "value": "int" },
      "range": {
        "end": { "character": 11, "line": 8 },
        "start": { "character": 9, "line": 8 }
      }
    }
    |}]
;;
