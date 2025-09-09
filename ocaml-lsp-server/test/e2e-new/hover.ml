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

let%expect_test "object method call" =
  let source =
    {ocaml|
let f (o : <  g : int -> unit >) = o#g 4
|ocaml}
  in
  let position = Position.create ~line:1 ~character:38 in
  let req client =
    let* resp = hover client position in
    let () = print_hover resp in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| no hover response |}]
;;
