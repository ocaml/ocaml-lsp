open Test.Import

let ocamlformat_config =
  {|break-cases=all
break-separators=before
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

let write_in_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc
;;

let setup_ocamlformat content =
  let tmpdir = Stdlib.Filename.temp_file "ocamllsp-test-" "" in
  Stdlib.Sys.remove tmpdir;
  Unix.mkdir tmpdir 0o700;
  let ocamlformat_path = Stdlib.Filename.concat tmpdir ".ocamlformat" in
  write_in_file ocamlformat_path content;
  tmpdir
;;

let iter_formatting source path =
  let makeRequest textDocument =
    let options = FormattingOptions.create ~tabSize:2 ~insertSpaces:true () in
    Lsp.Client_request.TextDocumentFormatting
      (DocumentFormattingParams.create ~textDocument ~options ())
  in
  Lsp_helpers.iter_lsp_response ~path ~makeRequest ~source
;;

let print_formatting_textedits = function
  | None -> print_endline "No formatting result"
  | Some [] -> print_endline "No formatting needed"
  | Some edits ->
    edits
    |> Ppx_yojson_conv_lib.Yojson_conv.yojson_of_list TextEdit.yojson_of_t
    |> Yojson.Safe.pretty_to_string ~std:false
    |> print_endline
;;

let print_formatting source path = iter_formatting source path print_formatting_textedits

let%expect_test "can format an ocaml impl file" =
  let source =
    {ocaml|let rec gcd a b =
  match (a, b) with
    | 0, n
  | n, 0 ->
    n
  | _, _ -> gcd a (b mod a)
|ocaml}
  in
  let path =
    Stdlib.Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.ml"
  in
  print_formatting source path;
  [%expect
    {|
    [
      {
        "newText": "  | 0, n\n",
        "range": {
          "end": { "character": 0, "line": 3 },
          "start": { "character": 0, "line": 2 }
        }
      }
    ]
    |}]
;;

let%expect_test "leaves unchanged files alone" =
  let source =
    {ocaml|let rec gcd a b =
  match (a, b) with
  | 0, n
  | n, 0 ->
    n
  | _, _ -> gcd a (b mod a)
|ocaml}
  in
  let path =
    Stdlib.Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.ml"
  in
  print_formatting source path;
  [%expect {| No formatting needed |}]
;;

let%expect_test "can format an ocaml intf file" =
  let source =
    {ocaml|module Test :           sig
  type t =
    | Foo
    | Bar
    | Baz
end
|ocaml}
  in
  let path =
    Stdlib.Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.mli"
  in
  print_formatting source path;
  [%expect
    {|
    [
      {
        "newText": "module Test : sig\n",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 0 }
        }
      }
    ]
    |}]
;;

let%expect_test "does not format ignored files" =
  let source =
    {ocaml|"let rec gcd a b = match (a, b) with
  | 0, n
  | n, 0 ->
    n
  | _, _ -> gcd a (b mod a)
|ocaml}
  in
  let tmpdir = setup_ocamlformat ocamlformat_config in
  let name = "dont_format_me.ml" in
  write_in_file (Stdlib.Filename.concat tmpdir ".ocamlformat-ignore") (name ^ "\n");
  let path = Stdlib.Filename.concat tmpdir name in
  print_formatting source path;
  [%expect {| No formatting needed |}]
;;
