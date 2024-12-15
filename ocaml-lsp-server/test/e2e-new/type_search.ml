open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Type_search

module Util = struct
  let call_search position query with_doc doc_format client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create text_document position 3 query with_doc doc_format
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req =
      Lsp.Client_request.UnknownRequest { meth = "ocamllsp/typeSearch"; params }
    in
    Client.request client req
  ;;

  let test ~line ~character ~query source ~with_doc ?(doc_format = None) () =
    let position = Position.create ~character ~line in
    let request client =
      let open Fiber.O in
      let+ response = call_search position query with_doc doc_format client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test
    "Polarity Search for a simple query that takes an int and returns a string with \
     documentation"
  =
  let source = "" in
  let line = 1 in
  let character = 0 in
  let doc_format = Some MarkupKind.Markdown in
  Util.test ~line ~character ~query:"-int +string" source ~with_doc:true ~doc_format ();
  [%expect
    {|
    [
      {
        "name": "Int.to_string",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 29, "line": 152 },
          "start": { "character": 0, "line": 152 }
        },
        "doc": {
          "kind": "markdown",
          "value": "`to_string x` is the written representation of `x` in decimal."
        },
        "cost": 4,
        "constructible": "Int.to_string _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": {
          "kind": "markdown",
          "value": "Return the string representation of an integer, in decimal."
        },
        "cost": 4,
        "constructible": "string_of_int _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": {
          "kind": "markdown",
          "value": "Return the string representation of an integer, in decimal."
        },
        "cost": 4,
        "constructible": "string_of_int _"
      }
    ]
    |}]
;;

let%expect_test
    "Polarity Search for a simple query that takes an int and returns a string with no \
     documentation"
  =
  let source = "" in
  let line = 1 in
  let character = 0 in
  Util.test ~line ~character ~query:"-int +string" source ~with_doc:false ();
  [%expect
    {|
    [
      {
        "name": "Int.to_string",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 29, "line": 152 },
          "start": { "character": 0, "line": 152 }
        },
        "doc": null,
        "cost": 4,
        "constructible": "Int.to_string _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": null,
        "cost": 4,
        "constructible": "string_of_int _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": null,
        "cost": 4,
        "constructible": "string_of_int _"
      }
    ] |}]
;;

let%expect_test
    "Type Search for a simple query that takes an int and returns a string with no \
     documentation"
  =
  let source = "" in
  let line = 1 in
  let character = 0 in
  Util.test ~line ~character ~query:"int -> string" source ~with_doc:false ();
  [%expect
    {|
    [
      {
        "name": "Int.to_string",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 29, "line": 152 },
          "start": { "character": 0, "line": 152 }
        },
        "doc": null,
        "cost": 0,
        "constructible": "Int.to_string _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": null,
        "cost": 0,
        "constructible": "string_of_int _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": null,
        "cost": 0,
        "constructible": "string_of_int _"
      }
    ] |}]
;;

let%expect_test
    "Type Search for a simple query that takes an int and returns a string with \
     documentation"
  =
  let source = "" in
  let line = 1 in
  let character = 0 in
  Util.test ~line ~character ~query:"int -> string" source ~with_doc:true ();
  [%expect
    {|
    [
      {
        "name": "Int.to_string",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 29, "line": 152 },
          "start": { "character": 0, "line": 152 }
        },
        "doc": {
          "kind": "plaintext",
          "value": "[to_string x] is the written representation of [x] in decimal."
        },
        "cost": 0,
        "constructible": "Int.to_string _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": {
          "kind": "plaintext",
          "value": "Return the string representation of an integer, in decimal."
        },
        "cost": 0,
        "constructible": "string_of_int _"
      },
      {
        "name": "string_of_int",
        "typ": "int -> string",
        "loc": {
          "end": { "character": 33, "line": 740 },
          "start": { "character": 0, "line": 740 }
        },
        "doc": {
          "kind": "plaintext",
          "value": "Return the string representation of an integer, in decimal."
        },
        "cost": 0,
        "constructible": "string_of_int _"
      }
    ]
    |}]
;;
