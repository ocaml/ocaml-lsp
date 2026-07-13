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
