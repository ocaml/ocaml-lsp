open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Merlin_call_compatible

let call_merlin_compatible client command args result_as_sexp =
  let uri = DocumentUri.of_path "test.ml" in
  let text_document = TextDocumentIdentifier.create ~uri in
  let params =
    Req.Request_params.create ~text_document ~result_as_sexp ~command ~args
    |> Req.Request_params.yojson_of_t
    |> Jsonrpc.Structured.t_of_yojson
    |> Option.some
  in
  let req =
    Lsp.Client_request.UnknownRequest { meth = "ocamllsp/merlinCallCompatible"; params }
  in
  Client.request client req
;;

let%expect_test "case-analysis on simple example" =
  let source = {|type t = {a: int * int; b: string}
let f ({a; b} : t) = assert false|} in
  let request client =
    let open Fiber.O in
    let args = [ "-start"; "2:9"; "-end"; "2:9" ] in
    let+ response = call_merlin_compatible client "case-analysis" args false in
    Test.print_result response
  in
  Helpers.test source request;
  [%expect
    {|
    {
      "resultAsSexp": false,
      "result": "{\"class\":\"return\",\"value\":[{\"start\":{\"line\":2,\"col\":8},\"end\":{\"line\":2,\"col\":9}},\"a = (_, _)\"]}"
    } |}]
;;

let%expect_test "case-analysis on empty example" =
  let source = {||} in
  let request client =
    let open Fiber.O in
    let args = [ "-start"; "2:9"; "-end"; "2:9" ] in
    let* response = call_merlin_compatible client "case-analysis" args false in
    let () = Test.print_result response in
    Fiber.return ()
  in
  Helpers.test source request;
  [%expect
    {|
    {
      "resultAsSexp": false,
      "result": "{\"class\":\"exception\",\"value\":\"Merlin_analysis.Destruct.Nothing_to_do\"}"
    } |}]
;;

let%expect_test "case-analysis on simple example with result as sexp" =
  let source = {|type t = {a: int * int; b: string}
let f ({a; b} : t) = assert false|} in
  let request client =
    let open Fiber.O in
    let args = [ "-start"; "2:9"; "-end"; "2:9" ] in
    let* response = call_merlin_compatible client "case-analysis" args true in
    let () = Test.print_result response in
    Fiber.return ()
  in
  Helpers.test source request;
  [%expect
    {|
    {
      "resultAsSexp": true,
      "result": "((assoc) (class . \"return\") (value ((assoc) (start (assoc) (line . 2) (col . 8)) (end (assoc) (line . 2) (col . 9))) \"a = (_, _)\"))"
    } |}]
;;

let%expect_test "errors: warning is shown" =
  let source = {|let () = match Some 3 with | None -> ()|} in
  let request client =
    let open Fiber.O in
    let args = [] in
    let* response = call_merlin_compatible client "errors" args false in
    let () = Test.print_result response in
    Fiber.return ()
  in
  Helpers.test source request;
  [%expect
    {|
    {
      "resultAsSexp": false,
      "result": "{\"class\":\"return\",\"value\":[{\"start\":{\"line\":1,\"col\":9},\"end\":{\"line\":1,\"col\":39},\"type\":\"warning\",\"sub\":[],\"valid\":true,\"message\":\"Warning 8: this pattern-matching is not exhaustive.\\nHere is an example of a case that is not matched:\\nSome _\"}]}"
    } |}]
;;

let%expect_test "errors: warning is disabled" =
  let source = {|let () = match Some 3 with | None -> ()|} in
  let request client =
    let open Fiber.O in
    let args = [ "-w"; "-8" ] in
    let* response = call_merlin_compatible client "errors" args false in
    let () = Test.print_result response in
    Fiber.return ()
  in
  Helpers.test source request;
  [%expect
    {|
    { "resultAsSexp": false, "result": "{\"class\":\"return\",\"value\":[]}" } |}]
;;
