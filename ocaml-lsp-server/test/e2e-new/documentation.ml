open Test.Import

module Util = struct
  let call_documentation ~position ?(identifier = None) ?(contentFormat = None)
      client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document =
      TextDocumentIdentifier.yojson_of_t @@ TextDocumentIdentifier.create ~uri
    in
    let position = Position.yojson_of_t position in
    let params =
      `Assoc
        (("position", position)
        :: (match identifier with
           | Some ident -> ("identifier", `String ident)
           | None -> ("identifier", `Null))
        :: (match contentFormat with
           | Some fmt -> ("contentFormat", `String fmt)
           | None -> ("contentFormat", `Null))
        :: [ ("textDocument", text_document) ])
    in
    let params = Some (Jsonrpc.Structured.t_of_yojson params) in
    let req =
      Lsp.Client_request.UnknownRequest
        { meth = "ocamllsp/documentation"; params }
    in
    Client.request client req

  let print_documentation result =
    result |> Yojson.Safe.pretty_to_string ~std:false |> print_endline

  let test ~line ~character ?identifier ?contentFormat source =
    let position = Position.create ~character ~line in
    let request client =
      let open Fiber.O in
      let+ response =
        call_documentation ~position ~identifier ~contentFormat client
      in
      print_documentation response
    in
    Helpers.test source request
end

let%expect_test "Documentation of simple type with no contentFormat and no \
                 identifier" =
  let source = "type tree (** This is a comment *)" in
  let line = 0 in
  let character = 7 in
  Util.test ~line ~character source;
  [%expect
    {| { "doc": { "kind": "plaintext", "value": "This is a comment" } } |}]

let%expect_test "Documentation of simple type with contentFormat set to \
                 markdown" =
  let source = "type tree (** This is another comment *)" in
  let line = 0 in
  let character = 7 in
  let contentFormat = "markdown" in
  Util.test ~line ~character ~contentFormat source;
  [%expect
    {| { "doc": { "kind": "markdown", "value": "This is another comment" } } |}]

let%expect_test "Documentation of simple type with an identifier and \
                 contentFormat" =
  let source =
    "{|type tree (** This is another comment *)\n\
    \  List.iter ~f:(fun x -> x*x) [2;4]|}"
  in
  let line = 0 in
  let character = 7 in
  let identifier = "List" in
  let contentFormat = "markdown" in
  Util.test ~line ~character ~identifier ~contentFormat source;
  [%expect
    {|
    {
      "doc": {
        "kind": "markdown",
        "value": "List operations.\n\n   Some functions are flagged as not tail-recursive.  A tail-recursive\n   function uses constant stack space, while a non-tail-recursive function\n   uses stack space proportional to the length of its list argument, which\n   can be a problem with very long lists.  When the function takes several\n   list arguments, an approximate formula giving stack usage (in some\n   unspecified constant unit) is shown in parentheses.\n\n   The above considerations can usually be ignored if your lists are not\n   longer than about 10000 elements.\n\n   The labeled version of this module can be used as described in the\n   {!StdLabels} module."
      }
    } |}]

let%expect_test "Documentation of simple type with an identifier and no \
                 contentFormat" =
  let source =
    "{|type tree (** This is another comment *)\n\
    \  List.iter ~f:(fun x -> x*x) [2;4]|}"
  in
  let line = 0 in
  let character = 7 in
  let identifier = "List" in
  Util.test ~line ~character ~identifier source;
  [%expect
    {|
    {
      "doc": {
        "kind": "plaintext",
        "value": "List operations.\n\n   Some functions are flagged as not tail-recursive.  A tail-recursive\n   function uses constant stack space, while a non-tail-recursive function\n   uses stack space proportional to the length of its list argument, which\n   can be a problem with very long lists.  When the function takes several\n   list arguments, an approximate formula giving stack usage (in some\n   unspecified constant unit) is shown in parentheses.\n\n   The above considerations can usually be ignored if your lists are not\n   longer than about 10000 elements.\n\n   The labeled version of this module can be used as described in the\n   {!StdLabels} module."
      }
    } |}]

let%expect_test "Documentation when List module is shadowed" =
  let source =
    "{|\n\
     module List = struct\n\
    \  (** This is my custom list module *)\n\
    \  let rec iter ~f = function (** This is the custom iter module *)\n\
    \    | [] -> () (** This is when the list is empty *)\n\
    \    | x :: xs -> f x; iter ~f xs\n\
     end\n\
     List.iter ~f:(fun x -> x*x) [2;4]\n\
     |}"
  in
  let line = 1 in
  let character = 8 in
  Util.test ~line ~character source;
  [%expect
    {|
  {
    "doc": {
      "kind": "plaintext",
      "value": "List operations.\n\n   Some functions are flagged as not tail-recursive.  A tail-recursive\n   function uses constant stack space, while a non-tail-recursive function\n   uses stack space proportional to the length of its list argument, which\n   can be a problem with very long lists.  When the function takes several\n   list arguments, an approximate formula giving stack usage (in some\n   unspecified constant unit) is shown in parentheses.\n\n   The above considerations can usually be ignored if your lists are not\n   longer than about 10000 elements.\n\n   The labeled version of this module can be used as described in the\n   {!StdLabels} module."
    }
  } |}]
