open Test.Import

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover -> Lsp.Types.Hover.yojson_of_t hover |> Test.print_result
;;

let hover client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let markdown_capabilities =
  let hover =
    HoverClientCapabilities.create
      ~dynamicRegistration:true
      ~contentFormat:[ MarkupKind.Markdown; MarkupKind.PlainText ]
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~hover () in
  ClientCapabilities.create ~textDocument ()
;;

let documented_id_use_source =
  {ocaml|(** This function has a nice documentation *)
let id x = x

let () = id ()
|ocaml}
;;

let documented_div_use_source =
  {ocaml|(** This function has a nice documentation.

    It performs division of two integer numbers.

    @param x dividend
    @param divisor

    @return {i quotient}, i.e. result of division
    @raise Division_by_zero raised when divided by zero

    @see <https://en.wikipedia.org/wiki/Arithmetic#Division_(%C3%B7,_or_/)> article
    @see 'arithmetic.ml' for more context

    @since 4.0.0
    @before 4.4.0

    @deprecated use [(/)]

    @version 1.0.0
    @author John Doe *)
let div x y =
  x / y

let f = div 4 2
|ocaml}
;;
