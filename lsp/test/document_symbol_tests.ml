open Lsp.Types
module Position = Lsp.Position
module Range = Lsp.Range

let range start_line start_character end_line end_character =
  let start = Position.create ~line:start_line ~character:start_character in
  let end_ = Position.create ~line:end_line ~character:end_character in
  Range.create ~start ~end_
;;

let print_symbols symbols =
  let json = `List (List.map SymbolInformation.yojson_of_t symbols) in
  Yojson.Safe.pretty_to_string json |> print_endline
;;

let%expect_test "normalize document-symbol selection ranges" =
  let full_range = range 1 2 3 8 in
  let print label selection =
    let normalized =
      match selection with
      | None -> full_range
      | Some selection -> Lsp.Range.normalize_selection_range full_range ~selection
    in
    Printf.printf "%s: %s\n" label (Range.to_string normalized)
  in
  print "contained" (Some (range 1 4 2 6));
  print "contained empty" (Some (range 2 4 2 4));
  print "overlap before" (Some (range 0 9 1 5));
  print "overlap after" (Some (range 3 4 4 1));
  print "enclosing" (Some (range 0 0 4 0));
  print "touch before" (Some (range 0 0 1 2));
  print "touch after" (Some (range 3 8 4 0));
  print "disjoint before" (Some (range 0 0 1 1));
  print "disjoint after" (Some (range 3 9 4 0));
  print "ghost" None;
  print "reversed" (Some (range 2 6 2 4));
  [%expect
    {|
    contained: ((1, 4), (2, 6))
    contained empty: ((2, 4), (2, 4))
    overlap before: ((1, 2), (1, 5))
    overlap after: ((3, 4), (3, 8))
    enclosing: ((1, 2), (3, 8))
    touch before: ((1, 2), (3, 8))
    touch after: ((1, 2), (3, 8))
    disjoint before: ((1, 2), (3, 8))
    disjoint after: ((1, 2), (3, 8))
    ghost: ((1, 2), (3, 8))
    reversed: ((1, 2), (3, 8))
    |}]
;;

let%expect_test "flatten document symbols preserves hierarchy and tags" =
  let symbol ?children ?deprecated ?tags ~name ~kind ~range () =
    DocumentSymbol.create
      ?children
      ?deprecated
      ?tags
      ~name
      ~kind
      ~range
      ~selectionRange:range
      ()
  in
  let uri = Lsp.Uri.of_path "/workspace/test.ml" in
  let symbols =
    [ symbol
        ~name:"M"
        ~kind:SymbolKind.Module
        ~range:(range 0 0 10 0)
        ~children:
          [ symbol
              ~name:"x"
              ~kind:SymbolKind.Variable
              ~range:(range 1 0 1 1)
              ~deprecated:true
              ~tags:[ SymbolTag.Deprecated ]
              ()
          ; symbol ~name:"f" ~kind:SymbolKind.Function ~range:(range 2 0 2 5) ()
          ]
        ()
    ]
  in
  Lsp.Document_symbol.flatten ~uri symbols |> print_symbols;
  [%expect
    {|
    [
      {
        "kind": 2,
        "location": {
          "range": {
            "end": { "character": 0, "line": 10 },
            "start": { "character": 0, "line": 0 }
          },
          "uri": "file:///workspace/test.ml"
        },
        "name": "M"
      },
      {
        "containerName": "M",
        "deprecated": true,
        "kind": 13,
        "location": {
          "range": {
            "end": { "character": 1, "line": 1 },
            "start": { "character": 0, "line": 1 }
          },
          "uri": "file:///workspace/test.ml"
        },
        "name": "x",
        "tags": [ 1 ]
      },
      {
        "containerName": "M",
        "kind": 12,
        "location": {
          "range": {
            "end": { "character": 5, "line": 2 },
            "start": { "character": 0, "line": 2 }
          },
          "uri": "file:///workspace/test.ml"
        },
        "name": "f"
      }
    ]
    |}]
;;
