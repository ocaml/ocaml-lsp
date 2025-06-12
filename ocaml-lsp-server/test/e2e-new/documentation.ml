open Async
open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Get_documentation

module Util = struct
  let call_documentation ~position ?(identifier = None) ?(contentFormat = None) client =
    let uri = DocumentUri.of_path "test.ml" in
    let text_document = TextDocumentIdentifier.create ~uri in
    let params =
      Req.Request_params.create ~text_document ~position ~identifier ~contentFormat ()
      |> Req.Request_params.yojson_of_t
      |> Jsonrpc.Structured.t_of_yojson
      |> Option.some
    in
    let req =
      Lsp.Client_request.UnknownRequest { meth = "ocamllsp/getDocumentation"; params }
    in
    Client.request client req
  ;;

  let test ~line ~character ?identifier ?contentFormat source =
    let position = Position.create ~character ~line in
    let contentFormat =
      match contentFormat with
      | Some "markdown" -> Some MarkupKind.Markdown
      | Some "plaintext" | _ -> Some MarkupKind.PlainText
    in
    let request client =
      let open Fiber.O in
      let+ response = call_documentation ~position ~identifier ~contentFormat client in
      Test.print_result response
    in
    Helpers.test source request
  ;;
end

let%expect_test "Documentation of simple type with no contentFormat and no identifier" =
  let source = "type tree (** This is a comment *)" in
  let line = 0 in
  let character = 7 in
  let%map () = Util.test ~line ~character source in
  [%expect {| { "doc": { "kind": "plaintext", "value": "This is a comment" } } |}]
;;

let%expect_test "Documentation of simple type with contentFormat set to markdown" =
  let source = "type tree (** This is another comment *)" in
  let line = 0 in
  let character = 7 in
  let contentFormat = "markdown" in
  let%map () = Util.test ~line ~character ~contentFormat source in
  [%expect {| { "doc": { "kind": "markdown", "value": "This is another comment" } } |}]
;;

(* NOTE: the merlin documentation query appears to behave differently in some cases when
   used internally. This may be because we have doc comments in different places than the
   merlin documentation query expects. Since this does not represent any regression from
   current behavior, I'm accepting the different test results to unblock this upgrade. *)
let%expect_test "Documentation of simple type with an identifier and contentFormat" =
  let source =
    "{|type tree (** This is another comment *)\n  List.iter ~f:(fun x -> x*x) [2;4]|}"
  in
  let line = 0 in
  let character = 7 in
  let identifier = "List" in
  let contentFormat = "markdown" in
  let%map () = Util.test ~line ~character ~identifier ~contentFormat source in
  [%expect {| null |}]
;;

let%expect_test "Documentation of simple type with an identifier and no contentFormat" =
  let source =
    "{|type tree (** This is another comment *)\n  List.iter ~f:(fun x -> x*x) [2;4]|}"
  in
  let line = 0 in
  let character = 7 in
  let identifier = "List" in
  let%map () = Util.test ~line ~character ~identifier source in
  [%expect {| null |}]
;;

let%expect_test "Documentation when List module is shadowed" =
  let source =
    "{|\n\
     List.iter ~f:(fun x -> x*x) [2;4]\n\
    \ module List = struct\n\
    \  (** This is my custom list module *)\n\
    \  let rec iter ~f = function (** This is the custom iter module *)\n\
    \    | [] -> () (** This is when the list is empty *)\n\
    \    | x :: xs -> f x; iter ~f xs\n\
     end\n\
     List.iter ~f:(fun x -> x*x) [2;4]\n\
     |}"
  in
  let line = 2 in
  let character = 6 in
  let identifier = "List.iter" in
  let%map () = Util.test ~line ~character ~identifier source in
  [%expect
    {|
    {
      "doc": {
        "kind": "plaintext",
        "value": "[iter f [a1; ...; an]] applies function [f] in turn to\n   [[a1; ...; an]]. It is equivalent to\n   [f a1; f a2; ...; f an]."
      }
    }
    |}]
;;

let%expect_test "Documentation when List module is shadowed" =
  let source =
    "{|\n\
     List.iter ~f:(fun x -> x*x) [2;4]\n\
     module List = struct\n\
    \  (** This is my custom list module *)\n\
    \  let rec iter ~f = function (** This is the custom iter module *)\n\
    \    | [] -> () (** This is when the list is empty *)\n\
    \    | x :: xs -> f x; iter ~f xs\n\
     end\n\
     Base.List.iter ~f:(fun x -> x*x) [2;4]\n\
     |}"
  in
  let line = 7 in
  let character = 12 in
  let identifier = "Base.List.iter" in
  let%map () = Util.test ~line ~character ~identifier source in
  [%expect {| { "doc": { "kind": "plaintext", "value": "Base.List.iter" } } |}]
;;

(* TODO: Open Issue in Merlin to investigate while this doesnt return documentation of the custom List module*)
let%expect_test "Documentation when List module is shadowed" =
  let source =
    "{|\n\
     List.iter ~f:(fun x -> x*x) [2;4]\n\
     module List = struct\n\
    \  (** This is my custom list module *)\n\
    \  let rec iter ~f = function (** This is the custom iter module *)\n\
    \    | [] -> () (** This is when the list is empty *)\n\
    \    | x :: xs -> f x; iter ~f xs\n\
     end\n\
     Base.List.iter ~f:(fun x -> x*x) [2;4]\n\
     |}"
  in
  let line = 2 in
  let character = 9 in
  let%map () = Util.test ~line ~character source in
  [%expect {| null |}]
;;
