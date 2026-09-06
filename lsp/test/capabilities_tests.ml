open Base
open Lsp.Types
module Capabilities = Lsp.Capabilities

let%expect_test "completion snippet support is opt-in" =
  List.iter
    [ "no text document", {|{}|}
    ; "no completion", {|{"textDocument":{}}|}
    ; "no completion item", {|{"textDocument":{"completion":{}}}|}
    ; "absent", {|{"textDocument":{"completion":{"completionItem":{}}}}|}
    ; ( "false"
      , {|{"textDocument":{"completion":{"completionItem":{"snippetSupport":false}}}}|} )
    ; ( "true"
      , {|{"textDocument":{"completion":{"completionItem":{"snippetSupport":true}}}}|} )
    ]
    ~f:(fun (name, json) ->
      let capabilities = Yojson.Safe.from_string json |> ClientCapabilities.t_of_yojson in
      Stdlib.Printf.printf
        "%s: %b\n"
        name
        (Capabilities.completion_snippet_support capabilities));
  [%expect
    {|
    no text document: false
    no completion: false
    no completion item: false
    absent: false
    false: false
    true: true
    |}]
;;

let capabilities ?folding_range () =
  let textDocument =
    TextDocumentClientCapabilities.create ?foldingRange:folding_range ()
  in
  ClientCapabilities.create ~textDocument ()
;;

let print_folding_range (t : ClientCapabilities.t) =
  let line_folding_only = Capabilities.folding_range_line_folding_only t in
  let kinds =
    match Capabilities.folding_range_kinds t with
    | None -> "none"
    | Some kinds ->
      let kind_to_string = function
        | FoldingRangeKind.Comment -> "comment"
        | Imports -> "imports"
        | Region -> "region"
        | Other s -> s
      in
      kinds |> List.map ~f:kind_to_string |> String.concat ~sep:","
  in
  let limit =
    match Capabilities.folding_range_limit t with
    | None -> "none"
    | Some limit -> Int.to_string limit
  in
  Stdlib.Printf.printf
    "line_folding_only=%b kinds=%s limit=%s\n"
    line_folding_only
    kinds
    limit
;;

let%expect_test "folding range capabilities default to no support" =
  capabilities () |> print_folding_range;
  capabilities ~folding_range:(FoldingRangeClientCapabilities.create ()) ()
  |> print_folding_range;
  [%expect
    {|
    line_folding_only=false kinds=none limit=none
    line_folding_only=false kinds=none limit=none
    |}]
;;

let%expect_test "folding range capabilities are read" =
  let folding_range =
    FoldingRangeClientCapabilities.create
      ~lineFoldingOnly:true
      ~foldingRangeKind:
        (ClientFoldingRangeKindOptions.create ~valueSet:[ Region; Comment ] ())
      ~rangeLimit:100
      ()
  in
  capabilities ~folding_range () |> print_folding_range;
  [%expect
    {|
    line_folding_only=true kinds=region,comment limit=100
    |}]
;;

let%expect_test "folding range capabilities with absent optional fields" =
  let folding_range =
    FoldingRangeClientCapabilities.create
      ~foldingRangeKind:(ClientFoldingRangeKindOptions.create ())
      ()
  in
  capabilities ~folding_range () |> print_folding_range;
  [%expect
    {|
    line_folding_only=false kinds=none limit=none
    |}]
;;
