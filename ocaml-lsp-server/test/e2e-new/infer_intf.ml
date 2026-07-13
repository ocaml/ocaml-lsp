open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Infer_intf

let infer_intf client =
  let params =
    `List [ DocumentUri.yojson_of_t Helpers.uri ]
    |> Jsonrpc.Structured.t_of_yojson
    |> Option.some
  in
  Client.request client (UnknownRequest { meth = Req.meth; params })
;;

let%expect_test "can infer module interfaces" =
  let source =
    {ocaml|type t = Foo of int | Bar of bool

let f (x : t) = x
|ocaml}
  in
  let req client =
    let* response = infer_intf client in
    let () = Yojson.Safe.Util.to_string response |> print_string in
    Fiber.return ()
  in
  Helpers.test source req;
  [%expect
    {|
    type t = Foo of int | Bar of bool
    val f : t -> t |}]
;;
