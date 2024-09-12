open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Polarity_search

module Util = struct
  let call_search position query limit client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create text_document position limit query
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req =
      Lsp.Client_request.UnknownRequest { meth = "ocamllsp/polaritySearch"; params }
    in
    Client.request client req
  ;;

  let test ~line ~character ~query limit source =
    let position = Position.create ~character ~line in
    let request client =
      let open Fiber.O in
      let+ response = call_search position query limit client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Search for a simple query that takes an int and returns a string" =
  let source = "" in
  let line = 1 in
  let character = 0 in
  Util.test ~line ~character ~query:"-int +string" 20 source;
  [%expect
    {|
    [
      { "path": "string_of_int", "type": "int -> string" },
      { "path": "string_of_int", "type": "int -> string" },
      { "path": "Stdlib__Int.to_string", "type": "int -> string" },
      { "path": "really_input_string", "type": "in_channel -> int -> string" },
      { "path": "really_input_string", "type": "in_channel -> int -> string" },
      {
        "path": "Stdlib__Digest.channel",
        "type": "in_channel -> int -> Stdlib__Digest.t"
      },
      { "path": "Stdlib__String.make", "type": "int -> char -> string" },
      { "path": "Stdlib__StringLabels.make", "type": "int -> char -> string" },
      {
        "path": "Stdlib__Digest.BLAKE128.channel",
        "type": "in_channel -> int -> Stdlib__Digest.BLAKE128.t"
      },
      {
        "path": "Stdlib__Digest.BLAKE256.channel",
        "type": "in_channel -> int -> Stdlib__Digest.BLAKE256.t"
      },
      {
        "path": "Stdlib__Digest.BLAKE512.channel",
        "type": "in_channel -> int -> Stdlib__Digest.BLAKE512.t"
      },
      {
        "path": "Stdlib__Digest.MD5.channel",
        "type": "in_channel -> int -> Stdlib__Digest.MD5.t"
      },
      {
        "path": "Stdlib__In_channel.really_input_string",
        "type": "Stdlib__In_channel.t -> int -> string option"
      },
      {
        "path": "Stdlib__Printexc.Slot.format",
        "type": "int -> Stdlib__Printexc.Slot.t -> string option"
      },
      {
        "path": "Stdlib__Buffer.sub",
        "type": "Stdlib__Buffer.t -> int -> int -> string"
      },
      {
        "path": "Stdlib__Bytes.sub_string",
        "type": "bytes -> int -> int -> string"
      },
      {
        "path": "Stdlib__BytesLabels.sub_string",
        "type": "bytes -> pos:int -> len:int -> string"
      },
      {
        "path": "Stdlib__Digest.subbytes",
        "type": "bytes -> int -> int -> Stdlib__Digest.t"
      },
      {
        "path": "Stdlib__Digest.substring",
        "type": "string -> int -> int -> Stdlib__Digest.t"
      },
      {
        "path": "Stdlib__Lexing.sub_lexeme",
        "type": "Stdlib__Lexing.lexbuf -> int -> int -> string"
      }
    ] |}]
;;
