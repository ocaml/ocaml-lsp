open Test.Import
open Formatting

let ocamlformat_config =
  {|break-cases=all
break-separators=before
margin=60
break-sequences=true
cases-exp-indent=2
doc-comments=before
dock-collection-brackets=false
field-space=loose
if-then-else=k-r
indicate-nested-or-patterns=unsafe-no
let-and=sparse
sequence-style=terminator
space-around-arrays
space-around-lists
space-around-records
type-decl=sparse
wrap-comments=true
|}
;;

let iter_range_formatting source path range =
  let makeRequest textDocument =
    let options = FormattingOptions.create ~tabSize:2 ~insertSpaces:true () in
    Lsp.Client_request.TextDocumentRangeFormatting
      (DocumentRangeFormattingParams.create ~textDocument ~range ~options ())
  in
  Lsp_helpers.iter_lsp_response ~path ~makeRequest ~source
;;

let print_formatting source path range =
  iter_range_formatting source path range print_formatting_textedits
;;

let%expect_test "can format part of an ocaml impl file" =
  let source =
    {ocaml|let () =
  let rec gcd a b =
  match (a, b) with
    | 0, n
    | n, 0 ->
      n
    | _, _ -> gcd a (b mod a)
  in print_endline (string_of_int (gcd 48 18))
|ocaml}
  in
  let range =
    (* range selects entire definition of `gcd` *)
    Range.create
      ~start:(Position.create ~line:1 ~character:2)
      ~end_:(Position.create ~line:6 ~character:29)
  in
  let path = Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.ml" in
  print_formatting source path range;
  [%expect
    {|
    [
      {
        "newText": "    match (a, b) with\n",
        "range": {
          "end": { "character": 0, "line": 3 },
          "start": { "character": 0, "line": 2 }
        }
      }
    ]
    |}]
;;

let%expect_test "leaves formatted snippets alone" =
  let source =
    {ocaml|let () =
  let rec gcd a b =
    match (a, b) with
    | 0, n
    | n, 0 ->
      n
    | _, _ -> gcd a (b mod a)
  in print_endline (string_of_int (gcd 48 18))
|ocaml}
  in
  let range =
    (* range selects entire definition of `gcd` *)
    Range.create
      ~start:(Position.create ~line:1 ~character:2)
      ~end_:(Position.create ~line:6 ~character:29)
  in
  let path = Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.ml" in
  print_formatting source path range;
  [%expect {| No formatting needed |}]
;;

let%expect_test "formats semantic actions in ocamllex files" =
  let source =
    {ocamllex|{
  type Token = Char of char | Censored | EOF
}

rule censor = parse
  | "super secret"  { print_string "redacting input! it's too secretive"; Censored
                    }
  | _ as c          { Char c }
  | eof             { EOF }

{}
|ocamllex}
  in
  let range =
    (* range is selecting from `p` in `print_string` to `d` in `Censored` *)
    Range.create
      ~start:(Position.create ~line:5 ~character:22)
      ~end_:(Position.create ~line:5 ~character:82)
  in
  let path = Filename.concat (setup_ocamlformat ocamlformat_config) "lexer.mll" in
  print_formatting source path range;
  (* this also implicitly tests that `margin` is correctly read;
    if the value of 60 is not retrieved and 80 is used, the
    string would not be split across two lines *)
  [%expect
    {|
    [
      {
        "newText": "  | \"super secret\"  { print_string\n                        \"redacting input! it's too \\\n                         secretive\";\n                      Censored\n",
        "range": {
          "end": { "character": 0, "line": 6 },
          "start": { "character": 0, "line": 5 }
        }
      }
    ]
    |}]
;;

let%expect_test "does not format ignored files" =
  let source =
    {ocaml|let () =
  let rec gcd a b =
  match (a, b) with
    | 0, n
    | n, 0 ->
      n
    | _, _ -> gcd a (b mod a)
  in print_endline (string_of_int (gcd 48 18))
|ocaml}
  in
  let range =
    Range.create
      ~start:(Position.create ~line:1 ~character:2)
      ~end_:(Position.create ~line:6 ~character:29)
  in
  let tmpdir = setup_ocamlformat ocamlformat_config in
  let name = "dont_format_me.ml" in
  write_in_file (Filename.concat tmpdir ".ocamlformat-ignore") (name ^ "\n");
  let path = Filename.concat tmpdir name in
  print_formatting source path range;
  [%expect {| No formatting needed |}]
;;
