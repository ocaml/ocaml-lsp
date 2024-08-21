open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Merlin_jump

module Util = struct
  let call_jump position target client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create ~text_document ~position ~target ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req = Lsp.Client_request.UnknownRequest { meth = "ocamllsp/jump"; params } in
    Client.request client req
  ;;

  let test ~line ~character ~target ~source =
    let position = Position.create ~character ~line in
    let request client =
      let open Fiber.O in
      let+ response = call_jump position target client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Get location of a simple let target" =
  let source =
  "{|
  let find_vowel x =
  match x with
  | 'A' ->
    true
  | 'E' ->
    true
  | 'I' ->
    true
  | 'O' ->
    true
  | 'U' ->
    true
  | _ ->
    false
  |}" in
  let line = 4 in
  let character = 2 in
  let target = "match" in
  Util.test ~line ~character ~target ~source;
  [%expect {|
   |}]
;;
