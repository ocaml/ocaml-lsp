open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Construct

module Util = struct
  let call_construct ?depth ?with_values client position =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create ?depth ?with_values ~text_document ~position ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let test ?depth ?with_values ~line ~character source =
    let position = Position.create ~line ~character in
    let request client =
      let open Fiber.O in
      let+ response = call_construct ?depth ?with_values client position in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Example sample from merlin 1" =
  let source =
    {|type r = {the_t: t; id: int}
and t = A | B of r option

let x : t = _
|}
  in
  let line = 3
  and character = 13 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 13, "line": 3 },
        "start": { "character": 12, "line": 3 }
      },
      "result": [ "A", "(B _)" ]
    } |}]
;;

let%expect_test "Example sample from merlin 2" =
  let source =
    {|type r = {the_t: t; id: int}
and t = A | B of r option

let x : t = B _
|}
  in
  let line = 3
  and character = 14 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 15, "line": 3 },
        "start": { "character": 14, "line": 3 }
      },
      "result": [ "None", "(Some _)" ]
    } |}]
;;

let%expect_test "Example sample from merlin 3" =
  let source =
    {|type r = {the_t: t; id: int}
and t = A | B of r option

let x : t = B (Some _)
|}
  in
  let line = 3
  and character = 20 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 21, "line": 3 },
        "start": { "character": 20, "line": 3 }
      },
      "result": [ "{ the_t = _; id = _ }" ]
    } |}]
;;

let%expect_test "Example sample from merlin 4" =
  let source =
    {|type r = {the_t: t; id: int}
and t = A | B of r option

let x : t = B (Some { the_t = _; id = _ })
|}
  in
  let line = 3
  and character = 30 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 31, "line": 3 },
        "start": { "character": 30, "line": 3 }
      },
      "result": [ "A", "(B _)" ]
    } |}]
;;

let%expect_test "Example sample from merlin 5" =
  let source =
    {|type r = {the_t: t; id: int}
and t = A | B of r option

let x : t = B (Some { the_t = A; id = _ })
|}
  in
  let line = 3
  and character = 38 in
  Util.test ~line ~character source;
  [%expect
    {|
    {
      "position": {
        "end": { "character": 39, "line": 3 },
        "start": { "character": 38, "line": 3 }
      },
      "result": [ "0" ]
    } |}]
;;
