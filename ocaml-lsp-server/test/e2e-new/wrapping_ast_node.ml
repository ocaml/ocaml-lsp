open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Wrapping_ast_node

let wrapping_ast_node client position =
  let params =
    `Assoc
      [ "uri", DocumentUri.yojson_of_t Helpers.uri
      ; "position", Position.yojson_of_t position
      ]
    |> Jsonrpc.Structured.t_of_yojson
    |> Option.some
  in
  Client.request client (UnknownRequest { meth = Req.meth; params })
;;

let print_json json = Yojson.Safe.pretty_to_string ~std:false json |> print_endline

let test source position =
  let req client =
    let* response = wrapping_ast_node client position in
    print_json response;
    Fiber.return ()
  in
  Helpers.test source req
;;

let code_snippet_0 =
  {ocaml|let k = 1

module M = struct
  let a =
    let b = 1 in
    b + 1

  let c = 2
end
|ocaml}
;;

let%expect_test "empty document" =
  test "" (Position.create ~line:0 ~character:0);
  [%expect {| null |}]
;;

let%expect_test "when on a toplevel let binding" =
  test code_snippet_0 (Position.create ~line:0 ~character:5);
  [%expect
    {|
    {
      "end": { "character": 9, "line": 0 },
      "start": { "character": 0, "line": 0 }
    } |}]
;;
