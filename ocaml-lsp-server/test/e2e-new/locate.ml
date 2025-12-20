open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Locate

module Util = struct
  let call_locate ?prefix ?(kind = `Definition) ~position client =
    let text_document =
      TextDocumentIdentifier.create ~uri:(DocumentUri.of_path "test.ml")
    in
    let params =
      Req.Request_params.create ~text_document ?prefix ~kind ~position ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = Req.meth; params } in
    Client.request client req
  ;;

  let sanitize_path = function
    | `List ranges ->
      `List
        (List.map
           ~f:(function
             | `Assoc [ range; ("uri", `String value) ] ->
               let new_value =
                 value
                 |> String.split_on_char ~sep:'/'
                 |> List.last
                 |> function
                 | None -> "unknown"
                 | Some x -> "file:///" ^ x
               in
               `Assoc [ range; "uri", `String new_value ]
             | x -> x)
           ranges)
    | x -> x
  ;;

  let test ?prefix ?(kind = `Definition) ~line ~character source =
    let position = Position.create ~line ~character in
    let request client =
      let open Fiber.O in
      let+ response = call_locate ?prefix ~kind ~position client in
      Test.print_result (sanitize_path response)
    in
    Helpers.test source request
  ;;
end

let%expect_test "Locate identifier - 1" =
  let source = {| let x = 10 let y = 11 |}
  and line = 0
  and character = 23 in
  Util.test ~line ~character ~prefix:"x" source;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 5, "line": 0 },
          "start": { "character": 5, "line": 0 }
        },
        "uri": "file:///test.ml"
      }
    ]
    |}]
;;

let%expect_test "Locate identifier - 2" =
  let source = {| let x = 10 let y = 11 let z = 10 |}
  and line = 0
  and character = 23 in
  Util.test ~line ~character ~prefix:"y" source;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 16, "line": 0 },
          "start": { "character": 16, "line": 0 }
        },
        "uri": "file:///test.ml"
      }
    ]
    |}]
;;

let%expect_test "Locate identifier - 3" =
  let source = {| let x = 10 let y = 11 |}
  and line = 0
  and character = 23 in
  Util.test ~line ~character ~prefix:"List.map" source;
  [%expect
    {|
    [
      {
        "range": {
          "end": { "character": 8, "line": 89 },
          "start": { "character": 8, "line": 89 }
        },
        "uri": "file:///list.ml"
      }
    ]
    |}]
;;
