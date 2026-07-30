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

let setup_ocamlformat content =
  let tmpdir = Test.temp_dir "ocamllsp-test-" in
  let ocamlformat_path = Filename.concat tmpdir ".ocamlformat" in
  Test.write_file ocamlformat_path content;
  tmpdir
;;

let make_request textDocument =
  let options = FormattingOptions.create ~tabSize:2 ~insertSpaces:true () in
  Lsp.Client_request.TextDocumentFormatting
    (DocumentFormattingParams.create ~textDocument ~options ())
;;

let iter_formatting ?language_id source path =
  Lsp_helpers.iter_lsp_response
    ~language_id:(Option.value language_id ~default:"ocaml")
    ~path
    ~makeRequest:make_request
    ~source
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

let print_formatting ?language_id source path =
  iter_formatting ?language_id source path print_formatting_textedits
;;

let print_formatting_error ?language_id source path =
  Lsp_helpers.iter_lsp_response_result
    ~language_id:(Option.value language_id ~default:"ocaml")
    ~path
    ~makeRequest:make_request
    ~source
    (function
    | Error error -> Jsonrpc.Response.Error.yojson_of_t error |> Test.print_result
    | Ok _ -> print_endline "Expected formatting to fail")
;;

let print_request_error = function
  | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
    ->
    Printf.printf
      "code=%s message=%s\n"
      (Jsonrpc.Response.Error.Code.to_string error.code)
      error.message;
    Fiber.return ()
  | Error errors -> Fiber.reraise_all errors
  | Ok _ ->
    print_endline "formatting unexpectedly succeeded";
    Fiber.return ()
;;

let test_formatter_failure ~path_env source =
  let handler = Client.Handler.make ~on_notification:(fun _ _ -> Fiber.return ()) () in
  Test.run_initialized ~handler ~extra_env:[ "PATH=" ^ path_env ]
  @@ fun client ->
  let uri = DocumentUri.of_path "/workspace/format_failure.ml" in
  let* () = Test.open_document ~client ~uri ~source () in
  let textDocument = TextDocumentIdentifier.create ~uri in
  let* result =
    Fiber.collect_errors (fun () -> Client.request client (make_request textDocument))
  in
  let* () = print_request_error result in
  Test.exit_client client
;;

let%expect_test "reports a missing ocamlformat executable" =
  let empty_path = Test.temp_dir "ocamllsp-no-ocamlformat-" in
  test_formatter_failure ~path_env:empty_path "let  x=1";
  [%expect
    {|
    code=InvalidRequest message=Unable to find ocamlformat binary. You need to install ocamlformat manually to use the formatting feature.
    |}]
;;

let%expect_test "reports a nonzero ocamlformat exit" =
  let failing_path = Test.temp_dir "ocamllsp-failing-ocamlformat-" in
  let formatter = Filename.concat failing_path "ocamlformat" in
  Test.write_file
    formatter
    "#!/bin/sh\n\
     while IFS= read -r line; do :; done\n\
     echo formatter exploded >&2\n\
     exit 7\n";
  Unix.chmod formatter 0o700;
  test_formatter_failure ~path_env:failing_path "let  x=1";
  [%expect {| code=InternalError message=formatter exploded |}]
;;

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
  let path = Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.ml" in
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
  let path = Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.ml" in
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
  let path = Filename.concat (setup_ocamlformat ocamlformat_config) "format_me.mli" in
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
  Test.write_file (Filename.concat tmpdir ".ocamlformat-ignore") (name ^ "\n");
  let path = Filename.concat tmpdir name in
  print_formatting source path;
  [%expect {| No formatting needed |}]
;;

let%expect_test "does not format unsupported documents" =
  let test language_id path source =
    print_endline language_id;
    print_formatting ~language_id source path
  in
  test
    "ocaml.ocamllex"
    "lexer.mll"
    {|rule token = parse
  | eof { EOF }
|};
  test
    "ocaml.menhir"
    "parser.mly"
    {|%token EOF
%%
main:
  | EOF { () }
|};
  test
    "cram"
    "test.t"
    {|  $ echo hello
  hello
|};
  [%expect
    {|
    ocaml.ocamllex
    No formatting result
    ocaml.menhir
    No formatting result
    cram
    No formatting result
    |}]
;;

let%expect_test "routes dune documents through dune" =
  print_formatting_error ~language_id:"dune" "(library)" "dune";
  [%expect
    {|
    {
      "code": -32600,
      "message": "No dune instance found. Please run dune in watch mode for /dune"
    }
    |}]
;;
