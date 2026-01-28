open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Locate_types

module Util = struct
  let call ~position client =
    let text_document =
      TextDocumentIdentifier.create ~uri:(DocumentUri.of_path "test.ml")
    in
    let params =
      Req.Request_params.create ~text_document ~position ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let test ~line ~character source =
    let position = Position.create ~line ~character in
    let request client =
      let open Fiber.O in
      let+ response = call ~position client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Locate types - simple type" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "a",
        "result": {
          "kind": "found",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        }
      },
      "children": []
    }
    |}]
;;

let%expect_test "Locate types - parametrized type" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a one_arg= assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "one_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "one_arg",
        "result": {
          "kind": "found",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - parametrized type 2" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : (a, b) two_arg = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        },
        {
          "type": "two_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 14, "line": 5 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "two_arg",
        "result": {
          "kind": "found",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 14, "line": 5 }
        }
      },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - arrow type" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a -> b -> c = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        },
        {
          "type": "c",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 3 }
        }
      ],
      "data": { "kind": "arrow" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "c",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 3 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - arrow type with label" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : x:a -> ?y:b -> c = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        },
        {
          "type": "c",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 3 }
        }
      ],
      "data": { "kind": "arrow" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "c",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 3 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - triple" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a * b * c = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        },
        {
          "type": "c",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 3 }
        }
      ],
      "data": { "kind": "tuple" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "c",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 3 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - pair" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a * b = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        }
      ],
      "data": { "kind": "tuple" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - tycon" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : _ one_arg = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "one_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "one_arg",
        "result": {
          "kind": "found",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      },
      "children": [
        { "data": { "kind": "other", "result": "'a" }, "children": [] }
      ]
    }
    |}]
;;

let%expect_test "Locate types - tycon + tyvar" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : 'a one_arg = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "one_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "one_arg",
        "result": {
          "kind": "found",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      },
      "children": [
        { "data": { "kind": "other", "result": "'a" }, "children": [] }
      ]
    }
    |}]
;;

let%expect_test "Locate types - objects" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : <x: a; y: b> = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        }
      ],
      "data": { "kind": "object" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - polyvar 1" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : [`A of a | `B of b ] = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        }
      ],
      "data": { "kind": "poly-variant" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - polyvar 2" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : [< `A of a | `B of b ] = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        }
      ],
      "data": { "kind": "poly-variant" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - polyvar 3" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : [> `A of a | `B of b ] = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        }
      ],
      "data": { "kind": "poly-variant" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - polyvar 4" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : [`B | `A of a ] = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        }
      ],
      "data": { "kind": "poly-variant" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - polyvar 4" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : [`B | `A ] = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect {| { "set": [], "data": { "kind": "poly-variant" }, "children": [] } |}]
;;

let%expect_test "Locate types - 1" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : [ ] = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect {| { "set": [], "data": { "kind": "poly-variant" }, "children": [] } |}]
;;

let%expect_test "Locate types - polyvar of tuples" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : [`S of (a * b) ] = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        }
      ],
      "data": { "kind": "poly-variant" },
      "children": [
        {
          "data": { "kind": "tuple" },
          "children": [
            {
              "data": {
                "kind": "type-ref",
                "type": "a",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 1 }
                }
              },
              "children": []
            },
            {
              "data": {
                "kind": "type-ref",
                "type": "b",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 2 }
                }
              },
              "children": []
            }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - builtin" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : string = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [],
      "data": {
        "kind": "type-ref",
        "type": "string",
        "result": { "kind": "built-in", "type": "string" }
      },
      "children": []
    }
    |}]
;;

let%expect_test "Locate types - builtin 2" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : int = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [],
      "data": {
        "kind": "type-ref",
        "type": "int",
        "result": { "kind": "built-in", "type": "int" }
      },
      "children": []
    }
    |}]
;;

let%expect_test "Locate types - builtin + tyvar" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a option = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "option",
        "result": { "kind": "built-in", "type": "option" }
      },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - builtin + tyvar (list)" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a list = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "list",
        "result": { "kind": "built-in", "type": "list" }
      },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - builtin + tyvar of builtin" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a option one_arg list = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "one_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "list",
        "result": { "kind": "built-in", "type": "list" }
      },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "one_arg",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 8, "line": 4 }
            }
          },
          "children": [
            {
              "data": {
                "kind": "type-ref",
                "type": "option",
                "result": { "kind": "built-in", "type": "option" }
              },
              "children": [
                {
                  "data": {
                    "kind": "type-ref",
                    "type": "a",
                    "result": {
                      "kind": "found",
                      "has_uri": true,
                      "uri": "file:///test.ml",
                      "position": { "character": 5, "line": 1 }
                    }
                  },
                  "children": []
                }
              ]
            }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - parametrized over tuple" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : (a * b) one_arg = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        },
        {
          "type": "one_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "one_arg",
        "result": {
          "kind": "found",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      },
      "children": [
        {
          "data": { "kind": "tuple" },
          "children": [
            {
              "data": {
                "kind": "type-ref",
                "type": "a",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 1 }
                }
              },
              "children": []
            },
            {
              "data": {
                "kind": "type-ref",
                "type": "b",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 2 }
                }
              },
              "children": []
            }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - parametrized over tycon and tyvar" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : (a option, _) two_arg = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "two_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 14, "line": 5 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "two_arg",
        "result": {
          "kind": "found",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 14, "line": 5 }
        }
      },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "option",
            "result": { "kind": "built-in", "type": "option" }
          },
          "children": [
            {
              "data": {
                "kind": "type-ref",
                "type": "a",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 1 }
                }
              },
              "children": []
            }
          ]
        },
        { "data": { "kind": "other", "result": "'a" }, "children": [] }
      ]
    }
    |}]
;;

let%expect_test "Locate types - arrow of tuple" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : a -> b -> (a * b) = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        }
      ],
      "data": { "kind": "arrow" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "b",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 2 }
            }
          },
          "children": []
        },
        {
          "data": { "kind": "tuple" },
          "children": [
            {
              "data": {
                "kind": "type-ref",
                "type": "a",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 1 }
                }
              },
              "children": []
            },
            {
              "data": {
                "kind": "type-ref",
                "type": "b",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 2 }
                }
              },
              "children": []
            }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - nested tycon" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg
type aliased = t
let () =
  let foo : ((a one_arg) list) option = assert false in
  ()
"|}
  and line = 8
  and character = 7 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "one_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 8, "line": 4 }
        }
      ],
      "data": {
        "kind": "type-ref",
        "type": "option",
        "result": { "kind": "built-in", "type": "option" }
      },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "list",
            "result": { "kind": "built-in", "type": "list" }
          },
          "children": [
            {
              "data": {
                "kind": "type-ref",
                "type": "one_arg",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 8, "line": 4 }
                }
              },
              "children": [
                {
                  "data": {
                    "kind": "type-ref",
                    "type": "a",
                    "result": {
                      "kind": "found",
                      "has_uri": true,
                      "uri": "file:///test.ml",
                      "position": { "character": 5, "line": 1 }
                    }
                  },
                  "children": []
                }
              ]
            }
          ]
        }
      ]
    }
    |}]
;;

let%expect_test "Locate types - arrow of nested tycon" =
  let source =
    {|
type a
type b
type c
type 'a one_arg
type ('a, 'b) two_arg

let f : a -> (a, b * c) two_arg = fun x -> assert false
"|}
  and line = 7
  and character = 4 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "set": [
        {
          "type": "a",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 1 }
        },
        {
          "type": "b",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 2 }
        },
        {
          "type": "c",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 5, "line": 3 }
        },
        {
          "type": "two_arg",
          "has_uri": true,
          "uri": "file:///test.ml",
          "position": { "character": 14, "line": 5 }
        }
      ],
      "data": { "kind": "arrow" },
      "children": [
        {
          "data": {
            "kind": "type-ref",
            "type": "a",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 5, "line": 1 }
            }
          },
          "children": []
        },
        {
          "data": {
            "kind": "type-ref",
            "type": "two_arg",
            "result": {
              "kind": "found",
              "has_uri": true,
              "uri": "file:///test.ml",
              "position": { "character": 14, "line": 5 }
            }
          },
          "children": [
            {
              "data": {
                "kind": "type-ref",
                "type": "a",
                "result": {
                  "kind": "found",
                  "has_uri": true,
                  "uri": "file:///test.ml",
                  "position": { "character": 5, "line": 1 }
                }
              },
              "children": []
            },
            {
              "data": { "kind": "tuple" },
              "children": [
                {
                  "data": {
                    "kind": "type-ref",
                    "type": "b",
                    "result": {
                      "kind": "found",
                      "has_uri": true,
                      "uri": "file:///test.ml",
                      "position": { "character": 5, "line": 2 }
                    }
                  },
                  "children": []
                },
                {
                  "data": {
                    "kind": "type-ref",
                    "type": "c",
                    "result": {
                      "kind": "found",
                      "has_uri": true,
                      "uri": "file:///test.ml",
                      "position": { "character": 5, "line": 3 }
                    }
                  },
                  "children": []
                }
              ]
            }
          ]
        }
      ]
    }
    |}]
;;
