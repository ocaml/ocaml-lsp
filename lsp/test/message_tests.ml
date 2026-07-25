open Lsp
open Types

let print_json json = Yojson.Safe.pretty_to_string json |> print_endline

let%expect_test "prepare type hierarchy request round trip" =
  let uri = DocumentUri.of_string "file:///workspace/test.ml" in
  let params =
    TypeHierarchyPrepareParams.create
      ~position:(Position.create ~line:2 ~character:4)
      ~textDocument:(TextDocumentIdentifier.create ~uri)
      ()
  in
  let request = Client_request.TextDocumentPrepareTypeHierarchy params in
  let jsonrpc = Client_request.to_jsonrpc_request request ~id:(`Int 1) in
  Printf.printf "method: %s\n" jsonrpc.method_;
  (match Client_request.of_jsonrpc jsonrpc with
   | Ok
       (Client_request.E
          (Client_request.TextDocumentPrepareTypeHierarchy params as parsed_request)) ->
     print_endline "parsed typed request";
     print_json (TypeHierarchyPrepareParams.yojson_of_t params);
     let json = Client_request.yojson_of_result parsed_request None in
     print_json json;
     (match Client_request.response_of_json parsed_request json with
      | None -> print_endline "decoded result: none"
      | Some _ -> print_endline "decoded result: items")
   | Ok _ -> print_endline "parsed as the wrong request"
   | Error message -> Printf.printf "parse error: %s\n" message);
  [%expect
    {|
    method: textDocument/prepareTypeHierarchy
    parsed typed request
    {
      "position": { "character": 4, "line": 2 },
      "textDocument": { "uri": "file:///workspace/test.ml" }
    }
    null
    decoded result: none
    |}]
;;

let%expect_test "log trace notification round trip" =
  let notification =
    Server_notification.LogTrace
      (LogTraceParams.create ~message:"indexing" ~verbose:"indexing module A" ())
  in
  let jsonrpc = Server_notification.to_jsonrpc notification in
  Printf.printf "method: %s\n" jsonrpc.method_;
  (match Server_notification.of_jsonrpc jsonrpc with
   | Ok (Server_notification.LogTrace params) ->
     print_endline "parsed typed notification";
     print_json (LogTraceParams.yojson_of_t params)
   | Ok _ -> print_endline "parsed as the wrong notification"
   | Error message -> Printf.printf "parse error: %s\n" message);
  [%expect
    {|
    method: $/logTrace
    parsed typed notification
    { "message": "indexing", "verbose": "indexing module A" }
    |}]
;;
