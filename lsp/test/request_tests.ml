open Lsp
open Types

let print_json json = Yojson.Safe.pretty_to_string json |> Stdlib.print_endline

let request_payload method_ params =
  let fields = [ "method", `String method_ ] in
  let fields =
    match params with
    | None -> fields
    | Some params -> fields @ [ "params", (params :> Yojson.Safe.t) ]
  in
  `Assoc fields
;;

let content_uri = DocumentUri.of_string "ocaml-source://example"

let%expect_test "workspace text document content request round trip" =
  let request =
    Client_request.WorkspaceTextDocumentContent
      (TextDocumentContentParams.create ~uri:content_uri)
  in
  let jsonrpc = Client_request.to_jsonrpc_request request ~id:(`Int 1) in
  print_json (request_payload jsonrpc.method_ jsonrpc.params);
  (match Client_request.of_jsonrpc jsonrpc with
   | Ok
       (Client_request.E
          (Client_request.WorkspaceTextDocumentContent params as parsed_request)) ->
     Stdlib.Printf.printf "parsed typed request: %s\n" (DocumentUri.to_string params.uri);
     let result = TextDocumentContentResult.create ~text:"let x = 1\n" in
     let json = Client_request.yojson_of_result parsed_request result in
     print_json json;
     let decoded = Client_request.response_of_json parsed_request json in
     Stdlib.Printf.printf "decoded result: %S\n" decoded.text
   | Ok _ -> Stdlib.print_endline "parsed as the wrong request"
   | Error message -> Stdlib.Printf.printf "parse error: %s\n" message);
  [%expect
    {|
    {
      "method": "workspace/textDocumentContent",
      "params": { "uri": "ocaml-source://example" }
    }
    parsed typed request: ocaml-source://example
    { "text": "let x = 1\n" }
    decoded result: "let x = 1\n"
    |}]
;;

let%expect_test "workspace text document content refresh request round trip" =
  let request =
    Server_request.WorkspaceTextDocumentContentRefresh
      (TextDocumentContentRefreshParams.create ~uri:content_uri)
  in
  let jsonrpc = Server_request.to_jsonrpc_request request ~id:(`Int 1) in
  print_json (request_payload jsonrpc.method_ jsonrpc.params);
  (match Server_request.of_jsonrpc jsonrpc with
   | Ok
       (Server_request.E
          (Server_request.WorkspaceTextDocumentContentRefresh params as parsed_request))
     ->
     Stdlib.Printf.printf "parsed typed request: %s\n" (DocumentUri.to_string params.uri);
     let json = Server_request.yojson_of_result parsed_request () in
     print_json json;
     let () = Server_request.response_of_json parsed_request json in
     Stdlib.print_endline "decoded result"
   | Ok _ -> Stdlib.print_endline "parsed as the wrong request"
   | Error message -> Stdlib.Printf.printf "parse error: %s\n" message);
  [%expect
    {|
    {
      "method": "workspace/textDocumentContent/refresh",
      "params": { "uri": "ocaml-source://example" }
    }
    parsed typed request: ocaml-source://example
    null
    decoded result
    |}]
;;
