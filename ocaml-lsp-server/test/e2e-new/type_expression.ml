open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Type_expression

module Util = struct
  let call_type_expr ~position ~expression client =
    let text_document =
      TextDocumentIdentifier.create ~uri:(DocumentUri.of_path "test.ml")
    in
    let params =
      Req.Request_params.create ~text_document ~position ~expression
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let test ~line ~character ~expression source =
    let position = Position.create ~line ~character in
    let request client =
      let open Fiber.O in
      let+ response = call_type_expr ~position ~expression client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Type an expression - 1" =
  let source = {| let x = 10 let y = 11 |}
  and line = 0
  and character = 9
  and expression = {|"foo"|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "string" |}]
;;

let%expect_test "Type an expression - 2" =
  let source = {| let x = 10 let y = function `Foo -> () | _ -> () |}
  and line = 1
  and character = 0
  and expression = {|List.map|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "('a -> 'b) -> 'a list -> 'b list" |}]
;;

let%expect_test "Type an expression - with unbound value" =
  let source = {| let x = 10 let y = function `Foo -> () | _ -> () |}
  and line = 1
  and character = 0
  and expression = {|z|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "Unbound value z" |}]
;;

let%expect_test "Type an expression - with menhir error" =
  let source = {| let x = 10 let y = function `Foo -> () | _ -> () |}
  and line = 1
  and character = 0
  and expression = {|('a, ) list|} in
  Util.test ~line ~character ~expression source;
  [%expect {| "Ocaml_preprocess.Parser_raw.MenhirBasics.Error" |}]
;;
