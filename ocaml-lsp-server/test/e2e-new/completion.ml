open Test.Import

let iter_completions
  ?prep
  ?path
  ?(triggerCharacter = "")
  ?(triggerKind = CompletionTriggerKind.Invoked)
  ~position
  =
  let makeRequest textDocument =
    let context = CompletionContext.create ~triggerCharacter ~triggerKind () in
    Lsp.Client_request.TextDocumentCompletion
      (CompletionParams.create ~textDocument ~position ~context ())
  in
  Lsp_helpers.iter_lsp_response ?prep ?path ~makeRequest
;;

let print_completions
  ?(prep = fun _ -> Fiber.return ())
  ?(path = "foo.ml")
  ?(limit = 10)
  ?(pre_print = fun x -> x)
  source
  position
  =
  iter_completions ~prep ~path ~source ~position (function
    | None -> print_endline "No completion Items"
    | Some completions ->
      let items =
        match completions with
        | `CompletionList comp -> comp.items
        | `List comp -> comp
      in
      items
      |> pre_print
      |> (function
       | [] -> print_endline "No completions"
       | items ->
         print_endline "Completions:";
         let originalLength = List.length items in
         items
         |> List.take (min limit originalLength)
         |> List.iter ~f:(fun item ->
           item
           |> CompletionItem.yojson_of_t
           |> Yojson.Safe.pretty_to_string ~std:false
           |> print_endline);
         if originalLength > limit then print_endline "............."))
;;

let%expect_test "can start completion at arbitrary position (before the dot)" =
  let source = {ocaml|Strin.func|ocaml} in
  let position = Position.create ~line:0 ~character:5 in
  print_completions source position;
  [%expect
    {|
      Completions:
      {
        "detail": "",
        "kind": 9,
        "label": "String",
        "sortText": "0000",
        "textEdit": {
          "newText": "String",
          "range": {
            "end": { "character": 5, "line": 0 },
            "start": { "character": 0, "line": 0 }
          }
        }
      }
      {
        "detail": "",
        "kind": 9,
        "label": "StringLabels",
        "sortText": "0001",
        "textEdit": {
          "newText": "StringLabels",
          "range": {
            "end": { "character": 5, "line": 0 },
            "start": { "character": 0, "line": 0 }
          }
        }
      } |}]
;;

let%expect_test "can start completion at arbitrary position" =
  let source = {ocaml|StringLabels|ocaml} in
  let position = Position.create ~line:0 ~character:6 in
  print_completions source position;
  [%expect
    {|
      Completions:
      {
        "detail": "",
        "kind": 9,
        "label": "String",
        "sortText": "0000",
        "textEdit": {
          "newText": "String",
          "range": {
            "end": { "character": 6, "line": 0 },
            "start": { "character": 0, "line": 0 }
          }
        }
      }
      {
        "detail": "",
        "kind": 9,
        "label": "StringLabels",
        "sortText": "0001",
        "textEdit": {
          "newText": "StringLabels",
          "range": {
            "end": { "character": 6, "line": 0 },
            "start": { "character": 0, "line": 0 }
          }
        }
      } |}]
;;

let%expect_test "can start completion at arbitrary position 2" =
  let source = {ocaml|StringLabels|ocaml} in
  let position = Position.create ~line:0 ~character:7 in
  print_completions source position;
  [%expect
    {|
    Completions:
    {
      "detail": "",
      "kind": 9,
      "label": "StringLabels",
      "sortText": "0000",
      "textEdit": {
        "newText": "StringLabels",
        "range": {
          "end": { "character": 7, "line": 0 },
          "start": { "character": 0, "line": 0 }
        }
      }
    } |}]
;;

let%expect_test "can start completion after operator without space" =
  let source = {ocaml|[1;2]|>List.ma|ocaml} in
  let position = Position.create ~line:0 ~character:14 in
  print_completions source position;
  [%expect
    {|
      Completions:
      {
        "detail": "('a -> 'b) -> 'a list -> 'b list",
        "kind": 12,
        "label": "map",
        "sortText": "0000",
        "textEdit": {
          "newText": "map",
          "range": {
            "end": { "character": 14, "line": 0 },
            "start": { "character": 12, "line": 0 }
          }
        }
      }
      {
        "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
        "kind": 12,
        "label": "mapi",
        "sortText": "0001",
        "textEdit": {
          "newText": "mapi",
          "range": {
            "end": { "character": 14, "line": 0 },
            "start": { "character": 12, "line": 0 }
          }
        }
      }
      {
        "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
        "kind": 12,
        "label": "map2",
        "sortText": "0002",
        "textEdit": {
          "newText": "map2",
          "range": {
            "end": { "character": 14, "line": 0 },
            "start": { "character": 12, "line": 0 }
          }
        }
      } |}]
;;

let%expect_test "can start completion after operator with space" =
  let source = {ocaml|[1;2] |> List.ma|ocaml} in
  let position = Position.create ~line:0 ~character:16 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "('a -> 'b) -> 'a list -> 'b list",
    "kind": 12,
    "label": "map",
    "sortText": "0000",
    "textEdit": {
      "newText": "map",
      "range": {
        "end": { "character": 16, "line": 0 },
        "start": { "character": 14, "line": 0 }
      }
    }
  }
  {
    "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
    "kind": 12,
    "label": "mapi",
    "sortText": "0001",
    "textEdit": {
      "newText": "mapi",
      "range": {
        "end": { "character": 16, "line": 0 },
        "start": { "character": 14, "line": 0 }
      }
    }
  }
  {
    "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
    "kind": 12,
    "label": "map2",
    "sortText": "0002",
    "textEdit": {
      "newText": "map2",
      "range": {
        "end": { "character": 16, "line": 0 },
        "start": { "character": 14, "line": 0 }
      }
    }
  }
  |}]
;;

let%expect_test "can start completion in dot chain with tab" =
  let source = {ocaml|[1;2] |> List.	ma|ocaml} in
  let position = Position.create ~line:0 ~character:17 in
  print_completions source position;
  [%expect
    {|
      Completions:
      {
        "detail": "('a -> 'b) -> 'a list -> 'b list",
        "kind": 12,
        "label": "map",
        "sortText": "0000",
        "textEdit": {
          "newText": "map",
          "range": {
            "end": { "character": 17, "line": 0 },
            "start": { "character": 15, "line": 0 }
          }
        }
      }
      {
        "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
        "kind": 12,
        "label": "mapi",
        "sortText": "0001",
        "textEdit": {
          "newText": "mapi",
          "range": {
            "end": { "character": 17, "line": 0 },
            "start": { "character": 15, "line": 0 }
          }
        }
      }
      {
        "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
        "kind": 12,
        "label": "map2",
        "sortText": "0002",
        "textEdit": {
          "newText": "map2",
          "range": {
            "end": { "character": 17, "line": 0 },
            "start": { "character": 15, "line": 0 }
          }
        }
      }
  |}]
;;

let%expect_test "can start completion in dot chain with newline" =
  let source = {ocaml|[1;2] |> List.
ma|ocaml} in
  let position = Position.create ~line:1 ~character:2 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "('a -> 'b) -> 'a list -> 'b list",
    "kind": 12,
    "label": "map",
    "sortText": "0000",
    "textEdit": {
      "newText": "map",
      "range": {
        "end": { "character": 2, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    }
  }
  {
    "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
    "kind": 12,
    "label": "mapi",
    "sortText": "0001",
    "textEdit": {
      "newText": "mapi",
      "range": {
        "end": { "character": 2, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    }
  }
  {
    "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
    "kind": 12,
    "label": "map2",
    "sortText": "0002",
    "textEdit": {
      "newText": "map2",
      "range": {
        "end": { "character": 2, "line": 1 },
        "start": { "character": 0, "line": 1 }
      }
    }
  }
  |}]
;;

let%expect_test "can start completion in dot chain with space" =
  let source = {ocaml|[1;2] |> List. ma|ocaml} in
  let position = Position.create ~line:0 ~character:17 in
  print_completions source position;
  [%expect
    {|
      Completions:
      {
        "detail": "('a -> 'b) -> 'a list -> 'b list",
        "kind": 12,
        "label": "map",
        "sortText": "0000",
        "textEdit": {
          "newText": "map",
          "range": {
            "end": { "character": 17, "line": 0 },
            "start": { "character": 15, "line": 0 }
          }
        }
      }
      {
        "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
        "kind": 12,
        "label": "mapi",
        "sortText": "0001",
        "textEdit": {
          "newText": "mapi",
          "range": {
            "end": { "character": 17, "line": 0 },
            "start": { "character": 15, "line": 0 }
          }
        }
      }
      {
        "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
        "kind": 12,
        "label": "map2",
        "sortText": "0002",
        "textEdit": {
          "newText": "map2",
          "range": {
            "end": { "character": 17, "line": 0 },
            "start": { "character": 15, "line": 0 }
          }
        }
      }
  |}]
;;

let%expect_test "can start completion after dereference" =
  let source = {ocaml|let apple=ref 10 in
!ap|ocaml} in
  let position = Position.create ~line:1 ~character:3 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "int ref",
    "kind": 12,
    "label": "apple",
    "sortText": "0000",
    "textEdit": {
      "newText": "apple",
      "range": {
        "end": { "character": 3, "line": 1 },
        "start": { "character": 1, "line": 1 }
      }
    }
  }
  |}]
;;

let%expect_test "can complete symbol passed as a named argument" =
  let source = {ocaml|let g ~f = f 0 in
g ~f:ig|ocaml} in
  let position = Position.create ~line:1 ~character:7 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "'a -> unit",
    "kind": 12,
    "label": "ignore",
    "sortText": "0000",
    "textEdit": {
      "newText": "ignore",
      "range": {
        "end": { "character": 7, "line": 1 },
        "start": { "character": 5, "line": 1 }
      }
    }
  }
  |}]
;;

let%expect_test "can complete symbol passed as a named argument - 2" =
  let source =
    {ocaml|module M = struct let igfoo _x = () end
let g ~f = f 0 in
g ~f:M.ig|ocaml}
  in
  let position = Position.create ~line:2 ~character:9 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "'a -> unit",
    "kind": 12,
    "label": "igfoo",
    "sortText": "0000",
    "textEdit": {
      "newText": "igfoo",
      "range": {
        "end": { "character": 9, "line": 2 },
        "start": { "character": 7, "line": 2 }
      }
    }
  }
  |}]
;;

let%expect_test "can complete symbol passed as an optional argument" =
  let source = {ocaml|
let g ?f = f in
g ?f:ig
    |ocaml} in
  let position = Position.create ~line:2 ~character:7 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "'a -> unit",
    "kind": 12,
    "label": "ignore",
    "sortText": "0000",
    "textEdit": {
      "newText": "ignore",
      "range": {
        "end": { "character": 7, "line": 2 },
        "start": { "character": 5, "line": 2 }
      }
    }
  }
  |}]
;;

let%expect_test "can complete symbol passed as an optional argument - 2" =
  let source =
    {ocaml|module M = struct let igfoo _x = () end
let g ?f = f in
g ?f:M.ig|ocaml}
  in
  let position = Position.create ~line:2 ~character:9 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "'a -> unit",
    "kind": 12,
    "label": "igfoo",
    "sortText": "0000",
    "textEdit": {
      "newText": "igfoo",
      "range": {
        "end": { "character": 9, "line": 2 },
        "start": { "character": 7, "line": 2 }
      }
    }
  }
  |}]
;;

let%expect_test "completes identifier after completion-triggering character" =
  let source =
    {ocaml|
module Test = struct
  let somenum = 42
  let somestring = "hello"
end

let x = Test.
    |ocaml}
  in
  let position = Position.create ~line:6 ~character:13 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "int",
    "kind": 12,
    "label": "somenum",
    "sortText": "0000",
    "textEdit": {
      "newText": "somenum",
      "range": {
        "end": { "character": 13, "line": 6 },
        "start": { "character": 13, "line": 6 }
      }
    }
  }
  {
    "detail": "string",
    "kind": 12,
    "label": "somestring",
    "sortText": "0001",
    "textEdit": {
      "newText": "somestring",
      "range": {
        "end": { "character": 13, "line": 6 },
        "start": { "character": 13, "line": 6 }
      }
    }
  }
  |}]
;;

let%expect_test "completes infix operators" =
  let source = {ocaml|
let (>>|) = (+)
let y = 1 >
|ocaml} in
  let position = Position.create ~line:2 ~character:11 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "int -> int -> int",
    "kind": 12,
    "label": ">>|",
    "sortText": "0000",
    "textEdit": {
      "newText": ">>|",
      "range": {
        "end": { "character": 11, "line": 2 },
        "start": { "character": 10, "line": 2 }
      }
    }
  }
  {
    "detail": "'a -> 'a -> bool",
    "kind": 12,
    "label": ">",
    "sortText": "0001",
    "textEdit": {
      "newText": ">",
      "range": {
        "end": { "character": 11, "line": 2 },
        "start": { "character": 10, "line": 2 }
      }
    }
  }
  {
    "detail": "'a -> 'a -> bool",
    "kind": 12,
    "label": ">=",
    "sortText": "0002",
    "textEdit": {
      "newText": ">=",
      "range": {
        "end": { "character": 11, "line": 2 },
        "start": { "character": 10, "line": 2 }
      }
    }
  }
  |}]
;;

let%expect_test "completes without prefix" =
  let source =
    {ocaml|
let somenum = 42
let somestring = "hello"

let plus_42 (x:int) (y:int) =
  somenum +
|ocaml}
  in
  let position = Position.create ~line:5 ~character:12 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "int -> int -> int",
    "kind": 12,
    "label": "+",
    "sortText": "0000",
    "textEdit": {
      "newText": "+",
      "range": {
        "end": { "character": 12, "line": 5 },
        "start": { "character": 11, "line": 5 }
      }
    }
  }
  {
    "detail": "float -> float -> float",
    "kind": 12,
    "label": "+.",
    "sortText": "0001",
    "textEdit": {
      "newText": "+.",
      "range": {
        "end": { "character": 12, "line": 5 },
        "start": { "character": 11, "line": 5 }
      }
    }
  }
  |}]
;;

let%expect_test "completes labels" =
  let source = {ocaml|let f = ListLabels.map ~|ocaml} in
  let position = Position.create ~line:0 ~character:24 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "int -> int",
    "kind": 12,
    "label": "~+",
    "sortText": "0000",
    "textEdit": {
      "newText": "~+",
      "range": {
        "end": { "character": 24, "line": 0 },
        "start": { "character": 23, "line": 0 }
      }
    }
  }
  {
    "detail": "float -> float",
    "kind": 12,
    "label": "~+.",
    "sortText": "0001",
    "textEdit": {
      "newText": "~+.",
      "range": {
        "end": { "character": 24, "line": 0 },
        "start": { "character": 23, "line": 0 }
      }
    }
  }
  {
    "detail": "int -> int",
    "kind": 12,
    "label": "~-",
    "sortText": "0002",
    "textEdit": {
      "newText": "~-",
      "range": {
        "end": { "character": 24, "line": 0 },
        "start": { "character": 23, "line": 0 }
      }
    }
  }
  {
    "detail": "float -> float",
    "kind": 12,
    "label": "~-.",
    "sortText": "0003",
    "textEdit": {
      "newText": "~-.",
      "range": {
        "end": { "character": 24, "line": 0 },
        "start": { "character": 23, "line": 0 }
      }
    }
  }
  {
    "detail": "'a -> 'b",
    "kind": 5,
    "label": "~f",
    "sortText": "0004",
    "textEdit": {
      "newText": "~f",
      "range": {
        "end": { "character": 24, "line": 0 },
        "start": { "character": 23, "line": 0 }
      }
    }
  }
  |}]
;;

let%expect_test "works for polymorphic variants - function application context - 1" =
  let source =
    {ocaml|
let f (_a: [`String | `Int of int]) = ()

let u = f `Str
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:14 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "`String",
    "kind": 20,
    "label": "`String",
    "sortText": "0000",
    "textEdit": {
      "newText": "`String",
      "range": {
        "end": { "character": 14, "line": 3 },
        "start": { "character": 10, "line": 3 }
      }
    }
  }
  |}]
;;

let%expect_test "works for polymorphic variants - function application context - 2" =
  let source =
    {ocaml|
let f (_a: [`String | `Int of int]) = ()

let u = f `In
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:13 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "`Int of int",
    "kind": 20,
    "label": "`Int",
    "sortText": "0000",
    "textEdit": {
      "newText": "`Int",
      "range": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 10, "line": 3 }
      }
    }
  }
  |}]
;;

let%expect_test "works for polymorphic variants" =
  let source = {ocaml|
type t = [ `Int | `String ]

let x : t = `I
  |ocaml} in
  let position = Position.create ~line:3 ~character:15 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "`Int",
    "kind": 20,
    "label": "`Int",
    "sortText": "0000",
    "textEdit": {
      "newText": "`Int",
      "range": {
        "end": { "character": 15, "line": 3 },
        "start": { "character": 13, "line": 3 }
      }
    }
  }
  |}]
;;

let%expect_test "completion for holes" =
  let source = {ocaml|let u : int = _|ocaml} in
  let position = Position.create ~line:0 ~character:15 in
  let filter =
    List.filter ~f:(fun (item : CompletionItem.t) ->
      not (String.starts_with ~prefix:"__" item.label))
  in
  print_completions ~pre_print:filter source position;
  [%expect
    {|
  Completions:
  {
    "filterText": "_0",
    "kind": 1,
    "label": "0",
    "sortText": "0000",
    "textEdit": {
      "newText": "0",
      "range": {
        "end": { "character": 15, "line": 0 },
        "start": { "character": 14, "line": 0 }
      }
    }
  }
  |}]
;;

let%expect_test "completes identifier at top level" =
  let source =
    {ocaml|
let somenum = 42
let somestring = "hello"

let () =
  some
|ocaml}
  in
  let position = Position.create ~line:5 ~character:6 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "int",
    "kind": 12,
    "label": "somenum",
    "sortText": "0000",
    "textEdit": {
      "newText": "somenum",
      "range": {
        "end": { "character": 6, "line": 5 },
        "start": { "character": 2, "line": 5 }
      }
    }
  }
  {
    "detail": "string",
    "kind": 12,
    "label": "somestring",
    "sortText": "0001",
    "textEdit": {
      "newText": "somestring",
      "range": {
        "end": { "character": 6, "line": 5 },
        "start": { "character": 2, "line": 5 }
      }
    }
  }
  |}]
;;

let%expect_test "completes from a module" =
  let source = {ocaml|let f = List.m|ocaml} in
  let position = Position.create ~line:0 ~character:14 in
  print_completions source position;
  [%expect
    {|
  Completions:
  {
    "detail": "('a -> 'b) -> 'a list -> 'b list",
    "kind": 12,
    "label": "map",
    "sortText": "0000",
    "textEdit": {
      "newText": "map",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }
  {
    "detail": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
    "kind": 12,
    "label": "map2",
    "sortText": "0001",
    "textEdit": {
      "newText": "map2",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }
  {
    "detail": "(int -> 'a -> 'b) -> 'a list -> 'b list",
    "kind": 12,
    "label": "mapi",
    "sortText": "0002",
    "textEdit": {
      "newText": "mapi",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }
  {
    "detail": "'a -> 'a list -> bool",
    "kind": 12,
    "label": "mem",
    "sortText": "0003",
    "textEdit": {
      "newText": "mem",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }
  {
    "detail": "'a -> ('a * 'b) list -> bool",
    "kind": 12,
    "label": "mem_assoc",
    "sortText": "0004",
    "textEdit": {
      "newText": "mem_assoc",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }
  {
    "detail": "'a -> ('a * 'b) list -> bool",
    "kind": 12,
    "label": "mem_assq",
    "sortText": "0005",
    "textEdit": {
      "newText": "mem_assq",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }
  {
    "detail": "'a -> 'a list -> bool",
    "kind": 12,
    "label": "memq",
    "sortText": "0006",
    "textEdit": {
      "newText": "memq",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }
  {
    "detail": "('a -> 'a -> int) -> 'a list -> 'a list -> 'a list",
    "kind": 12,
    "label": "merge",
    "sortText": "0007",
    "textEdit": {
      "newText": "merge",
      "range": {
        "end": { "character": 14, "line": 0 },
        "start": { "character": 13, "line": 0 }
      }
    }
  }|}]
;;

let%expect_test "completes a module name" =
  let source = {ocaml|let f = L|ocaml} in
  let position = Position.create ~line:0 ~character:9 in
  print_completions ~pre_print:(List.take 5) source position;
  [%expect
    {|
  Completions:
  {
    "detail": "",
    "kind": 9,
    "label": "LargeFile",
    "sortText": "0000",
    "textEdit": {
      "newText": "LargeFile",
      "range": {
        "end": { "character": 9, "line": 0 },
        "start": { "character": 8, "line": 0 }
      }
    }
  }
  {
    "detail": "",
    "kind": 9,
    "label": "Lazy",
    "sortText": "0001",
    "textEdit": {
      "newText": "Lazy",
      "range": {
        "end": { "character": 9, "line": 0 },
        "start": { "character": 8, "line": 0 }
      }
    }
  }
  {
    "detail": "",
    "kind": 9,
    "label": "Lexing",
    "sortText": "0002",
    "textEdit": {
      "newText": "Lexing",
      "range": {
        "end": { "character": 9, "line": 0 },
        "start": { "character": 8, "line": 0 }
      }
    }
  }
  {
    "detail": "",
    "kind": 9,
    "label": "List",
    "sortText": "0003",
    "textEdit": {
      "newText": "List",
      "range": {
        "end": { "character": 9, "line": 0 },
        "start": { "character": 8, "line": 0 }
      }
    }
  }
  {
    "detail": "",
    "kind": 9,
    "label": "ListLabels",
    "sortText": "0004",
    "textEdit": {
      "newText": "ListLabels",
      "range": {
        "end": { "character": 9, "line": 0 },
        "start": { "character": 8, "line": 0 }
      }
    }
  }
  |}]
;;

let%expect_test "completion doesn't autocomplete record fields" =
  let source =
    {ocaml|
    type r = {
      x: int;
      y: string
    }

    let _ =
  |ocaml}
  in
  let position = Position.create ~line:5 ~character:8 in
  print_completions
    ~pre_print:
      (List.filter ~f:(fun (compl : CompletionItem.t) ->
         compl.label = "x" || compl.label = "y"))
    source
    position;
  (* We expect 0 completions*)
  [%expect {| No completions |}]
;;

let%expect_test "completion for `in` keyword - no prefix" =
  let source = {ocaml|
let foo param1 =
  let bar = param1 |ocaml} in
  let position = Position.create ~line:2 ~character:19 in
  print_completions ~limit:3 source position;
  [%expect
    {|
    Completions:
    {
      "kind": 14,
      "label": "in",
      "textEdit": {
        "newText": "in",
        "range": {
          "end": { "character": 19, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "'a -> 'b",
      "kind": 12,
      "label": "param1",
      "sortText": "0000",
      "textEdit": {
        "newText": "param1",
        "range": {
          "end": { "character": 19, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "'a ref -> 'a",
      "kind": 12,
      "label": "!",
      "sortText": "0001",
      "textEdit": {
        "newText": "!",
        "range": {
          "end": { "character": 19, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    ............. |}]
;;

let%expect_test "completion for `in` keyword - prefix i" =
  let source = {ocaml|
let foo param1 =
  let bar = param1 i
|ocaml} in
  let position = Position.create ~line:2 ~character:20 in
  print_completions ~limit:3 source position;
  [%expect
    {|
    Completions:
    {
      "kind": 14,
      "label": "in",
      "textEdit": {
        "newText": "in",
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "'a -> unit",
      "kind": 12,
      "label": "ignore",
      "sortText": "0000",
      "textEdit": {
        "newText": "ignore",
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "in_channel -> int",
      "kind": 12,
      "label": "in_channel_length",
      "sortText": "0001",
      "textEdit": {
        "newText": "in_channel_length",
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    ............. |}]
;;

let%expect_test "completion for `in` keyword - prefix in" =
  let source = {ocaml|
let foo param1 =
  let bar = param1 in
|ocaml} in
  let position = Position.create ~line:2 ~character:21 in
  print_completions ~limit:3 source position;
  [%expect
    {|
    Completions:
    {
      "kind": 14,
      "label": "in",
      "textEdit": {
        "newText": "in",
        "range": {
          "end": { "character": 21, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "in_channel -> int",
      "kind": 12,
      "label": "in_channel_length",
      "sortText": "0000",
      "textEdit": {
        "newText": "in_channel_length",
        "range": {
          "end": { "character": 21, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "int ref -> unit",
      "kind": 12,
      "label": "incr",
      "sortText": "0001",
      "textEdit": {
        "newText": "incr",
        "range": {
          "end": { "character": 21, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    ............. |}]
;;
