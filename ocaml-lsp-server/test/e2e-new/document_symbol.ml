open Test.Import

module Util = struct
  let call_document_symbol client =
    let uri = DocumentUri.of_path "test.ml" in
    let textDocument = TextDocumentIdentifier.create ~uri in
    let param = Lsp.Types.DocumentSymbolParams.create ~textDocument () in
    let req = Lsp.Client_request.DocumentSymbol param in
    Client.request client req
  ;;
end

let print_as_json result =
  result |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let print_result x =
  let result =
    match x with
    | Some (`DocumentSymbol xs) -> List.map ~f:DocumentSymbol.yojson_of_t xs
    | Some (`SymbolInformation xs) -> List.map ~f:SymbolInformation.yojson_of_t xs
    | None -> []
  in
  print_as_json (`List result)
;;

let%expect_test "documentOutline in an empty file" =
  let source = {||} in
  let request client =
    let open Fiber.O in
    let+ response = Util.call_document_symbol client in
    print_result response
  in
  Helpers.test source request;
  [%expect {| [] |}]
;;

let%expect_test "documentOutline with a module" =
  let source =
    {|
    module T = struct
      type t = int
    end              
  |}
  in
  let request client =
    let open Fiber.O in
    let+ response = Util.call_document_symbol client in
    print_result response
  in
  Helpers.test source request;
  [%expect
    {|
    [
      {
        "kind": 2,
        "location": {
          "range": {
            "end": { "character": 7, "line": 3 },
            "start": { "character": 4, "line": 1 }
          },
          "uri": "file:///test.ml"
        },
        "name": "T"
      },
      {
        "containerName": "T",
        "kind": 26,
        "location": {
          "range": {
            "end": { "character": 18, "line": 2 },
            "start": { "character": 6, "line": 2 }
          },
          "uri": "file:///test.ml"
        },
        "name": "t"
      }
    ]
    |}]
;;

let%expect_test "documentOutline with a module, a class and a class type" =
  let source =
    {|
    module T = struct
     type t = int
     class type b = object end
     end
     class c = object end
  |}
  in
  let request client =
    let open Fiber.O in
    let+ response = Util.call_document_symbol client in
    print_result response
  in
  Helpers.test source request;
  [%expect
    {|
    [
      {
        "kind": 2,
        "location": {
          "range": {
            "end": { "character": 8, "line": 4 },
            "start": { "character": 4, "line": 1 }
          },
          "uri": "file:///test.ml"
        },
        "name": "T"
      },
      {
        "containerName": "T",
        "kind": 26,
        "location": {
          "range": {
            "end": { "character": 17, "line": 2 },
            "start": { "character": 5, "line": 2 }
          },
          "uri": "file:///test.ml"
        },
        "name": "t"
      },
      {
        "containerName": "T",
        "kind": 11,
        "location": {
          "range": {
            "end": { "character": 30, "line": 3 },
            "start": { "character": 5, "line": 3 }
          },
          "uri": "file:///test.ml"
        },
        "name": "b"
      },
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 25, "line": 5 },
            "start": { "character": 5, "line": 5 }
          },
          "uri": "file:///test.ml"
        },
        "name": "c"
      }
    ]
    |}]
;;

let%expect_test "documentOutline with recursive definition" =
  let source =
    {|
     class a = object end and b = object end and c = object end
     class type ta = object end and tb = object end
  |}
  in
  let request client =
    let open Fiber.O in
    let+ response = Util.call_document_symbol client in
    print_result response
  in
  Helpers.test source request;
  [%expect
    {|
    [
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 25, "line": 1 },
            "start": { "character": 5, "line": 1 }
          },
          "uri": "file:///test.ml"
        },
        "name": "a"
      },
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 44, "line": 1 },
            "start": { "character": 26, "line": 1 }
          },
          "uri": "file:///test.ml"
        },
        "name": "b"
      },
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 63, "line": 1 },
            "start": { "character": 45, "line": 1 }
          },
          "uri": "file:///test.ml"
        },
        "name": "c"
      },
      {
        "kind": 11,
        "location": {
          "range": {
            "end": { "character": 31, "line": 2 },
            "start": { "character": 5, "line": 2 }
          },
          "uri": "file:///test.ml"
        },
        "name": "ta"
      },
      {
        "kind": 11,
        "location": {
          "range": {
            "end": { "character": 51, "line": 2 },
            "start": { "character": 32, "line": 2 }
          },
          "uri": "file:///test.ml"
        },
        "name": "tb"
      }
    ]
    |}]
;;

let%expect_test "documentOutline with recursive definition and methods" =
  let source =
    {|
     class a = object end
     and b = object
     val foo = 10
     method bar () = print_endline "bar"
     end and c = object end
     class type ta = object
        method baz : int -> int -> string
     end and tb = object end
  |}
  in
  let request client =
    let open Fiber.O in
    let+ response = Util.call_document_symbol client in
    print_result response
  in
  Helpers.test source request;
  [%expect
    {|
    [
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 25, "line": 1 },
            "start": { "character": 5, "line": 1 }
          },
          "uri": "file:///test.ml"
        },
        "name": "a"
      },
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 8, "line": 5 },
            "start": { "character": 5, "line": 2 }
          },
          "uri": "file:///test.ml"
        },
        "name": "b"
      },
      {
        "containerName": "b",
        "kind": 7,
        "location": {
          "range": {
            "end": { "character": 17, "line": 3 },
            "start": { "character": 5, "line": 3 }
          },
          "uri": "file:///test.ml"
        },
        "name": "foo"
      },
      {
        "containerName": "b",
        "kind": 6,
        "location": {
          "range": {
            "end": { "character": 40, "line": 4 },
            "start": { "character": 5, "line": 4 }
          },
          "uri": "file:///test.ml"
        },
        "name": "bar"
      },
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 27, "line": 5 },
            "start": { "character": 9, "line": 5 }
          },
          "uri": "file:///test.ml"
        },
        "name": "c"
      },
      {
        "kind": 11,
        "location": {
          "range": {
            "end": { "character": 8, "line": 8 },
            "start": { "character": 5, "line": 6 }
          },
          "uri": "file:///test.ml"
        },
        "name": "ta"
      },
      {
        "containerName": "ta",
        "kind": 6,
        "location": {
          "range": {
            "end": { "character": 41, "line": 7 },
            "start": { "character": 8, "line": 7 }
          },
          "uri": "file:///test.ml"
        },
        "name": "baz"
      },
      {
        "kind": 11,
        "location": {
          "range": {
            "end": { "character": 28, "line": 8 },
            "start": { "character": 9, "line": 8 }
          },
          "uri": "file:///test.ml"
        },
        "name": "tb"
      }
    ]
    |}]
;;

let%expect_test "documentOutline with nested recursive definition and methods" =
  let source =
    {|
     class a = object
     let a = object
     method inside_a_a () =
     let x_inside_a = 10 in
     print_int x
     end
     end
     and b = object
     val foo = 10
     method bar () = print_endline "bar"
     end and c = object end
     class type ta = object
        method baz : int -> int -> string
     end and tb = object end
     let final_let =
       let c = object method foo = 10 end in c
  |}
  in
  let request client =
    let open Fiber.O in
    let+ response = Util.call_document_symbol client in
    print_result response
  in
  Helpers.test source request;
  [%expect
    {|
    [
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 8, "line": 6 },
            "start": { "character": 5, "line": 1 }
          },
          "uri": "file:///test.ml"
        },
        "name": "a"
      },
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 8, "line": 11 },
            "start": { "character": 5, "line": 8 }
          },
          "uri": "file:///test.ml"
        },
        "name": "b"
      },
      {
        "containerName": "b",
        "kind": 7,
        "location": {
          "range": {
            "end": { "character": 17, "line": 9 },
            "start": { "character": 5, "line": 9 }
          },
          "uri": "file:///test.ml"
        },
        "name": "foo"
      },
      {
        "containerName": "b",
        "kind": 6,
        "location": {
          "range": {
            "end": { "character": 40, "line": 10 },
            "start": { "character": 5, "line": 10 }
          },
          "uri": "file:///test.ml"
        },
        "name": "bar"
      },
      {
        "kind": 5,
        "location": {
          "range": {
            "end": { "character": 27, "line": 11 },
            "start": { "character": 9, "line": 11 }
          },
          "uri": "file:///test.ml"
        },
        "name": "c"
      },
      {
        "kind": 11,
        "location": {
          "range": {
            "end": { "character": 8, "line": 14 },
            "start": { "character": 5, "line": 12 }
          },
          "uri": "file:///test.ml"
        },
        "name": "ta"
      },
      {
        "containerName": "ta",
        "kind": 6,
        "location": {
          "range": {
            "end": { "character": 41, "line": 13 },
            "start": { "character": 8, "line": 13 }
          },
          "uri": "file:///test.ml"
        },
        "name": "baz"
      },
      {
        "kind": 11,
        "location": {
          "range": {
            "end": { "character": 28, "line": 14 },
            "start": { "character": 9, "line": 14 }
          },
          "uri": "file:///test.ml"
        },
        "name": "tb"
      },
      {
        "kind": 13,
        "location": {
          "range": {
            "end": { "character": 46, "line": 16 },
            "start": { "character": 5, "line": 15 }
          },
          "uri": "file:///test.ml"
        },
        "name": "final_let"
      },
      {
        "containerName": "final_let",
        "kind": 13,
        "location": {
          "range": {
            "end": { "character": 41, "line": 16 },
            "start": { "character": 7, "line": 16 }
          },
          "uri": "file:///test.ml"
        },
        "name": "c"
      }
    ]
    |}]
;;
