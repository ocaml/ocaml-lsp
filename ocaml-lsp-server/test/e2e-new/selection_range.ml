open Test.Import

let selection_range client positions =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (SelectionRange (SelectionRangeParams.create ~textDocument ~positions ()))
;;

let print_selection_ranges ranges =
  let rec chain (selection_range : SelectionRange.t) =
    let rest =
      match selection_range.parent with
      | None -> []
      | Some parent -> chain parent
    in
    Range.yojson_of_t selection_range.range :: rest
  in
  `List (List.map ranges ~f:(fun range -> `List (chain range)))
  |> Yojson.Safe.pretty_to_string ~std:false
  |> print_endline
;;

let test source positions =
  let req client =
    let* response = selection_range client positions in
    print_selection_ranges response;
    Fiber.return ()
  in
  Helpers.test source req
;;

let%expect_test "returns a selection range for modules" =
  let source =
    {ocaml|let foo a b =
  let min_ab = min a b in
  let max_ab = max a b in
  min_ab * max_ab
|ocaml}
  in
  test source [ Position.create ~line:1 ~character:17 ];
  [%expect
    {|
    [
      [
        {
          "end": { "character": 18, "line": 1 },
          "start": { "character": 15, "line": 1 }
        },
        {
          "end": { "character": 22, "line": 1 },
          "start": { "character": 15, "line": 1 }
        },
        {
          "end": { "character": 22, "line": 1 },
          "start": { "character": 2, "line": 1 }
        },
        {
          "end": { "character": 17, "line": 3 },
          "start": { "character": 2, "line": 1 }
        },
        {
          "end": { "character": 17, "line": 3 },
          "start": { "character": 8, "line": 0 }
        },
        {
          "end": { "character": 17, "line": 3 },
          "start": { "character": 0, "line": 0 }
        }
      ]
    ]
    |}]
;;

let%expect_test "returns a selection range for more complex documents" =
  let source =
    {ocaml|type _ typ =
  | TInt : int typ
  | TBool : bool typ
module M = struct
  type t
  let f (_ : _ typ) = ()
end
|ocaml}
  in
  test source [ Position.create ~line:5 ~character:23 ];
  [%expect
    {|
    [
      [
        {
          "end": { "character": 24, "line": 5 },
          "start": { "character": 22, "line": 5 }
        },
        {
          "end": { "character": 24, "line": 5 },
          "start": { "character": 8, "line": 5 }
        },
        {
          "end": { "character": 24, "line": 5 },
          "start": { "character": 2, "line": 5 }
        },
        {
          "end": { "character": 24, "line": 5 },
          "start": { "character": 2, "line": 4 }
        },
        {
          "end": { "character": 3, "line": 6 },
          "start": { "character": 11, "line": 3 }
        },
        {
          "end": { "character": 3, "line": 6 },
          "start": { "character": 0, "line": 3 }
        },
        {
          "end": { "character": 3, "line": 6 },
          "start": { "character": 0, "line": 0 }
        }
      ]
    ]
    |}]
;;

let%expect_test "returns a selection range for functors" =
  let source =
    {ocaml|module M = Map.Make (struct
  type t = { o: < rank : int > }
  let compare a b = a.o#rank - b.o#rank
end)|ocaml}
  in
  test source [ Position.create ~line:2 ~character:26 ];
  [%expect
    {|
    [
      [
        {
          "end": { "character": 28, "line": 2 },
          "start": { "character": 23, "line": 2 }
        },
        {
          "end": { "character": 28, "line": 2 },
          "start": { "character": 20, "line": 2 }
        },
        {
          "end": { "character": 39, "line": 2 },
          "start": { "character": 20, "line": 2 }
        },
        {
          "end": { "character": 39, "line": 2 },
          "start": { "character": 14, "line": 2 }
        },
        {
          "end": { "character": 39, "line": 2 },
          "start": { "character": 2, "line": 2 }
        },
        {
          "end": { "character": 39, "line": 2 },
          "start": { "character": 2, "line": 1 }
        },
        {
          "end": { "character": 3, "line": 3 },
          "start": { "character": 21, "line": 0 }
        },
        {
          "end": { "character": 4, "line": 3 },
          "start": { "character": 11, "line": 0 }
        },
        {
          "end": { "character": 4, "line": 3 },
          "start": { "character": 0, "line": 0 }
        }
      ]
    ]
    |}]
;;

let%expect_test "returns a reasonable selection range for ill-typed modules" =
  let source =
    {ocaml|module M = struct
    let f x : int = string_of_int x
end|ocaml}
  in
  test source [ Position.create ~line:1 ~character:34 ];
  [%expect
    {|
    [
      [
        {
          "end": { "character": 35, "line": 1 },
          "start": { "character": 34, "line": 1 }
        },
        {
          "end": { "character": 35, "line": 1 },
          "start": { "character": 20, "line": 1 }
        },
        {
          "end": { "character": 35, "line": 1 },
          "start": { "character": 10, "line": 1 }
        },
        {
          "end": { "character": 35, "line": 1 },
          "start": { "character": 4, "line": 1 }
        },
        {
          "end": { "character": 3, "line": 2 },
          "start": { "character": 11, "line": 0 }
        },
        {
          "end": { "character": 3, "line": 2 },
          "start": { "character": 0, "line": 0 }
        }
      ]
    ]
    |}]
;;

let%expect_test "returns a reasonable selection range in the presence of syntax errors" =
  let source =
    {ocaml|module M = struct
    let f x : int = string_of_int x
ed|ocaml}
  in
  test source [ Position.create ~line:1 ~character:34 ];
  [%expect
    {|
    [
      [
        {
          "end": { "character": 35, "line": 1 },
          "start": { "character": 34, "line": 1 }
        },
        {
          "end": { "character": 2, "line": 2 },
          "start": { "character": 20, "line": 1 }
        },
        {
          "end": { "character": 2, "line": 2 },
          "start": { "character": 10, "line": 1 }
        },
        {
          "end": { "character": 2, "line": 2 },
          "start": { "character": 4, "line": 1 }
        },
        {
          "end": { "character": 2, "line": 2 },
          "start": { "character": 11, "line": 0 }
        },
        {
          "end": { "character": 2, "line": 2 },
          "start": { "character": 0, "line": 0 }
        }
      ]
    ]
    |}]
;;
