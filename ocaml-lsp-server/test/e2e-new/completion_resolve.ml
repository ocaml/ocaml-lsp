open Test.Import

let completion_item_resolve client label position =
  let data =
    `Assoc
      [ "textDocument", `Assoc [ "uri", DocumentUri.yojson_of_t Helpers.uri ]
      ; "position", Position.yojson_of_t position
      ]
  in
  Client.request client (CompletionItemResolve (CompletionItem.create ~label ~data ()))
;;

let print_completion_item item =
  CompletionItem.yojson_of_t item
  |> Yojson.Safe.pretty_to_string ~std:false
  |> print_endline
;;

let%expect_test "can get documentation for the end of document" =
  let source =
    {ocaml|List.ma
|ocaml}
  in
  let req client =
    let* response =
      completion_item_resolve client "map2" (Position.create ~line:0 ~character:5)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "documentation": "[map2 f [a1; ...; an] [b1; ...; bn]] is\n   [[f a1 b1; ...; f an bn]].\n   @raise Invalid_argument if the two lists are determined\n   to have different lengths.",
      "label": "map2"
    }
    |}]
;;

let%expect_test "can get documentation at arbitrary position" =
  let source =
    {ocaml|List.fld((=) 0) [1; 2; 3]
|ocaml}
  in
  let req client =
    let* response =
      completion_item_resolve client "find_all" (Position.create ~line:0 ~character:5)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    {
      "documentation": "[find_all] is another name for {!filter}.",
      "label": "find_all"
    }
    |}]
;;

let%expect_test "can get documentation at arbitrary position (before dot)" =
  let source =
    {ocaml|module Outer = struct
  (** documentation for [Inner] *)
  module Inner = struct
    let v = ()
  end
end

let _ = ();;

Outer.Inner.v
|ocaml}
  in
  let req client =
    let* response =
      completion_item_resolve client "Inner" (Position.create ~line:9 ~character:10)
    in
    print_completion_item response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| { "documentation": "documentation for [Inner]", "label": "Inner" } |}]
;;
