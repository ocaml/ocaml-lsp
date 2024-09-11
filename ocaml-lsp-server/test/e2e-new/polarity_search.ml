open Test.Import
module Req = Ocaml_lsp_server.Custom_request.PolaritySearch

module Util = struct
  let call_search position query client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create text_document position query
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req =
      Lsp.Client_request.UnknownRequest { meth = "ocamllsp/polaritySearch"; params }
    in
    Client.request client req
  ;;

  let test ~line ~character ~query source =
    let position = Position.create ~character ~line in
    let request client =
      let open Fiber.O in
      let+ response = call_search position query client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Search for a simple query that takes an int and returns a string" =
  let source = "" in
  let line = 1 in
  let character = 0 in
  Util.test ~line ~character ~query:"-int +string" source;
  [%expect {|
    [
      {
        "name": "string_of_int",
        "kind": 12,
        "desc": "int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "string_of_int",
        "kind": 12,
        "desc": "int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Int.to_string",
        "kind": 12,
        "desc": "int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "really_input_string",
        "kind": 12,
        "desc": "in_channel -> int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "really_input_string",
        "kind": 12,
        "desc": "in_channel -> int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.channel",
        "kind": 12,
        "desc": "in_channel -> int -> Stdlib__Digest.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__String.make",
        "kind": 12,
        "desc": "int -> char -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__StringLabels.make",
        "kind": 12,
        "desc": "int -> char -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE128.channel",
        "kind": 12,
        "desc": "in_channel -> int -> Stdlib__Digest.BLAKE128.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE256.channel",
        "kind": 12,
        "desc": "in_channel -> int -> Stdlib__Digest.BLAKE256.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE512.channel",
        "kind": 12,
        "desc": "in_channel -> int -> Stdlib__Digest.BLAKE512.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.MD5.channel",
        "kind": 12,
        "desc": "in_channel -> int -> Stdlib__Digest.MD5.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__In_channel.really_input_string",
        "kind": 12,
        "desc": "Stdlib__In_channel.t -> int -> string option",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Printexc.Slot.format",
        "kind": 12,
        "desc": "int -> Stdlib__Printexc.Slot.t -> string option",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Buffer.sub",
        "kind": 12,
        "desc": "Stdlib__Buffer.t -> int -> int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Bytes.sub_string",
        "kind": 12,
        "desc": "bytes -> int -> int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__BytesLabels.sub_string",
        "kind": 12,
        "desc": "bytes -> pos:int -> len:int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.subbytes",
        "kind": 12,
        "desc": "bytes -> int -> int -> Stdlib__Digest.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.substring",
        "kind": 12,
        "desc": "string -> int -> int -> Stdlib__Digest.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Lexing.sub_lexeme",
        "kind": 12,
        "desc": "Stdlib__Lexing.lexbuf -> int -> int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__String.init",
        "kind": 12,
        "desc": "int -> (int -> char) -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__String.sub",
        "kind": 12,
        "desc": "string -> int -> int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__StringLabels.init",
        "kind": 12,
        "desc": "int -> f:(int -> char) -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__StringLabels.sub",
        "kind": 12,
        "desc": "string -> pos:int -> len:int -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE128.subbytes",
        "kind": 12,
        "desc": "bytes -> int -> int -> Stdlib__Digest.BLAKE128.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE128.substring",
        "kind": 12,
        "desc": "string -> int -> int -> Stdlib__Digest.BLAKE128.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE256.subbytes",
        "kind": 12,
        "desc": "bytes -> int -> int -> Stdlib__Digest.BLAKE256.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE256.substring",
        "kind": 12,
        "desc": "string -> int -> int -> Stdlib__Digest.BLAKE256.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE512.subbytes",
        "kind": 12,
        "desc": "bytes -> int -> int -> Stdlib__Digest.BLAKE512.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.BLAKE512.substring",
        "kind": 12,
        "desc": "string -> int -> int -> Stdlib__Digest.BLAKE512.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.MD5.subbytes",
        "kind": 12,
        "desc": "bytes -> int -> int -> Stdlib__Digest.MD5.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Digest.MD5.substring",
        "kind": 12,
        "desc": "string -> int -> int -> Stdlib__Digest.MD5.t",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Lexing.sub_lexeme_opt",
        "kind": 12,
        "desc": "Stdlib__Lexing.lexbuf -> int -> int -> string option",
        "info": "",
        "deprecated": false
      },
      {
        "name": "CamlinternalOO.get_variables",
        "kind": 12,
        "desc": "CamlinternalOO.table -> string array -> int array",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Filename.temp_dir",
        "kind": 12,
        "desc": "?temp_dir:string -> ?perms:int -> string -> string -> string",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Arg.align",
        "kind": 12,
        "desc": "?limit:int ->\n(Stdlib__Arg.key * Stdlib__Arg.spec * Stdlib__Arg.doc) list ->\n(Stdlib__Arg.key * Stdlib__Arg.spec * Stdlib__Arg.doc) list",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Filename.open_temp_file",
        "kind": 12,
        "desc": "?mode:open_flag list ->\n?perms:int -> ?temp_dir:string -> string -> string -> string * out_channel",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Arg.parse_argv",
        "kind": 12,
        "desc": "?current:int ref ->\nstring array ->\n(Stdlib__Arg.key * Stdlib__Arg.spec * Stdlib__Arg.doc) list ->\nStdlib__Arg.anon_fun -> Stdlib__Arg.usage_msg -> unit",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Arg.parse_argv_dynamic",
        "kind": 12,
        "desc": "?current:int ref ->\nstring array ->\n(Stdlib__Arg.key * Stdlib__Arg.spec * Stdlib__Arg.doc) list ref ->\nStdlib__Arg.anon_fun -> string -> unit",
        "info": "",
        "deprecated": false
      },
      {
        "name": "Stdlib__Arg.parse_and_expand_argv_dynamic",
        "kind": 12,
        "desc": "int ref ->\nstring array ref ->\n(Stdlib__Arg.key * Stdlib__Arg.spec * Stdlib__Arg.doc) list ref ->\nStdlib__Arg.anon_fun -> string -> unit",
        "info": "",
        "deprecated": false
      }
    ] |}]
;;
