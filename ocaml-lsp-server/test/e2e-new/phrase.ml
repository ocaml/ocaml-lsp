open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Phrase

module Util = struct
  let call_phrase ~target ~position client =
    let text_document =
      TextDocumentIdentifier.create ~uri:(DocumentUri.of_path "test.ml")
    in
    let params =
      Req.Request_params.create ~text_document ~position ~target
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let test ~target ~line ~character source =
    let position = Position.create ~line ~character in
    let request client =
      let open Fiber.O in
      let+ response = call_phrase ~target ~position client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Jump to the next phrase - 1" =
  let source = {| let x = 10 let y = 11 |}
  and line = 0
  and character = 9
  and target = `Next in
  Util.test ~line ~character ~target source;
  [%expect {| { "character": 12, "line": 0 } |}]
;;

let%expect_test "Jump to the previous phrase - 1" =
  let source = {| let x = 10 let y = 11 |}
  and line = 0
  and character = 12
  and target = `Prev in
  Util.test ~line ~character ~target source;
  [%expect {| { "character": 1, "line": 0 } |}]
;;

let%expect_test "Jump to the next phrase - 2" =
  let source =
    {| module T = struct
    let x = 10
    let y = 11
end
let z = 10
module R = List    
|}
  and line = 1
  and character = 8
  and target = `Next in
  Util.test ~line ~character ~target source;
  [%expect {| { "character": 4, "line": 2 } |}]
;;

let%expect_test "Jump to the next phrase - 3" =
  let source =
    {| module T = struct
    let x = 10
    let y = 11
end
let z = 10
module R = List    
|}
  and line = 2
  and character = 4
  and target = `Next in
  Util.test ~line ~character ~target source;
  [%expect {| { "character": 0, "line": 4 } |}]
;;

let%expect_test "Jump to the next phrase - 4" =
  let source =
    {| module T = struct
    let x = 10
    let y = 11
end
let z = 10
module R = List    
|}
  and line = 4
  and character = 0
  and target = `Next in
  Util.test ~line ~character ~target source;
  [%expect {| { "character": 0, "line": 5 } |}]
;;

let%expect_test "Jump to the previous phrase - 2" =
  let source =
    {| module T = struct
    let x = 10
    let y = 11
end
let z = 10
module R = List    
|}
  and line = 5
  and character = 0
  and target = `Prev in
  Util.test ~line ~character ~target source;
  [%expect {| { "character": 0, "line": 4 } |}]
;;

let%expect_test "Jump to the previous phrase - 3" =
  let source =
    {| module T = struct
    let x = 10
    let y = 11
end
let z = 10
module R = List    
|}
  and line = 4
  and character = 0
  and target = `Prev in
  Util.test ~line ~character ~target source;
  [%expect {| { "character": 1, "line": 0 } |}]
;;
