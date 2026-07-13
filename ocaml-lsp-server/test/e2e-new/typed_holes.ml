open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Typed_holes

let typed_holes client =
  let params =
    `Assoc [ "uri", DocumentUri.yojson_of_t Helpers.uri ]
    |> Jsonrpc.Structured.t_of_yojson
    |> Option.some
  in
  Client.request client (UnknownRequest { meth = Req.meth; params })
;;

let print_json json = Yojson.Safe.pretty_to_string ~std:false json |> print_endline

let%expect_test "empty when no holes in file" =
  let source =
    {ocaml|let u = 1
|ocaml}
  in
  let req client =
    let* response = typed_holes client in
    print_json response;
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect {| [] |}]
;;
