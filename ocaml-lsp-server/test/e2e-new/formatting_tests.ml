open Test.Import

(** Tests ocamlformat integration *)

let write_to_file ~file_path ~contents =
  let file = open_out file_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr file)
    (fun () -> Printf.fprintf file "%s" contents)

(** this directory should also include [ocamlformat-rpc] if ocamlformat > 0.21.0 *)
let ofmt_bins =
  Bin.which "ocamlformat" ~path:Test._PATH
  |> Option.bind ~f:Path.parent |> Option.value_exn |> Path.to_string

let test ?dot_ocamlformat ?(with_ofmt_bin = true) ~src () =
  let src = String.trim src in
  let recvd_showMsg_notifs = Queue.create () in
  let handler =
    Client.Handler.make
      ~on_notification:
        (fun client -> function
          | Lsp.Server_notification.ShowMessage { message; type_ } ->
            Queue.push recvd_showMsg_notifs (message, type_);
            Fiber.return @@ Client.state client
          | _ -> Fiber.return ())
      ()
  in
  let extra_env =
    if with_ofmt_bin then
      let path = sprintf "PATH=%s" ofmt_bins in
      Some [ path ]
    else None
  in
  Test.run ?extra_env ~handler (fun client ->
      let proj_dir = mk_temp_dir ~pat:"ocaml_project" () in

      Option.iter dot_ocamlformat ~f:(fun contents ->
          let dot_ofmt = sprintf "%s/.ocamlformat" proj_dir in
          write_to_file ~file_path:dot_ofmt ~contents);

      let src_path = sprintf "%s/test.ml" proj_dir in
      write_to_file ~file_path:src_path ~contents:src;

      let run_client () =
        let client_capabilities = ClientCapabilities.create () in
        Client.start
          client
          (InitializeParams.create ~capabilities:client_capabilities ())
      in

      let run () =
        let* (_ : InitializeResult.t) = Client.initialized client in
        let uri = DocumentUri.of_path src_path in
        let* () =
          let textDocument =
            TextDocumentItem.create
              ~uri
              ~languageId:"ocaml"
              ~version:0
              ~text:src
          in
          Client.notification
            client
            (TextDocumentDidOpen
               (DidOpenTextDocumentParams.create ~textDocument))
        in
        let* resp =
          let textDocument = TextDocumentIdentifier.create ~uri in
          let params =
            DocumentFormattingParams.create
              ~textDocument
              ~options:
                (FormattingOptions.create ~tabSize:2 ~insertSpaces:true ())
              ()
          in

          Fiber.collect_errors (fun () ->
              Client.request client (TextDocumentFormatting params))
        in

        print_endline "* response:";
        (match resp with
        | Ok resp ->
          Option.map resp ~f:(fun edits ->
              List.map edits ~f:(fun edit ->
                  TextEdit.yojson_of_t edit |> Yojson.Safe.pretty_to_string)
              |> String.concat ~sep:"\n")
          |> Option.value ~default:"None"
          |> print_endline
        | Error ers ->
          List.iter ers ~f:(fun err ->
              print_endline @@ Printexc.to_string err.Exn_with_backtrace.exn));

        let* () = Client.request client Shutdown in

        print_endline "\n* showMessage notifications:";
        Queue.iter recvd_showMsg_notifs ~f:(fun (msg, _type) ->
            print_endline msg);

        Client.stop client
      in
      Fiber.fork_and_join_unit run_client run)

let dot_ocamlformat =
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
wrap-comments=true|}

let%expect_test "no ocamlformat and ocamlformat-rpc binaries" =
  test ~with_ofmt_bin:false ~dot_ocamlformat ~src:{|
let foo
=
bar|} ();
  [%expect
    {|
    * response:
    jsonrpc response error {
      "code": -32600,
      "message": "Unable to find ocamlformat binary. You need to install ocamlformat manually to use the formatting feature."
    }

    * showMessage notifications:
    Unable to find 'ocamlformat-rpc' binary. Types on hover may not be well-formatted. You need to install either 'ocamlformat' of version > 0.21.0 or, otherwise, 'ocamlformat-rpc' package.
    Unable to find ocamlformat binary. You need to install ocamlformat manually to use the formatting feature. |}]

(* FIXME *)
let%expect_test "when no .ocamlformat file exists in project, send \
                 notification about that" =
  test ~src:{|
let foo
=
bar|} ();
  [%expect {|
    * response:


    * showMessage notifications: |}]

let%expect_test "format simple code" =
  test ~dot_ocamlformat ~src:{|
let foo
=
bar|} ();
  [%expect
    {|
    * response:
    {
      "newText": "let foo = bar\n",
      "range": {
        "end": { "character": 0, "line": 3 },
        "start": { "character": 0, "line": 0 }
      }
    }

    * showMessage notifications: |}]

let%expect_test "format code with one syntax error" =
  test ~dot_ocamlformat ~src:(String.trim {|let foo
  bar|}) ();
  [%expect
    {|
      * response:
      jsonrpc response error {
        "code": -32603,
        "message": "ocamlformat: syntax error on line 2, characters 5-5"
      }

      * showMessage notifications:
      ocamlformat: syntax error on line 2, characters 5-5 |}]

let%expect_test "format code with two syntax errors" =
  test ~dot_ocamlformat ~src:{|
let foo =

type t = A of |} ();
  [%expect
    {|
      * response:
      jsonrpc response error {
        "code": -32603,
        "message": "ocamlformat: syntax error on line 3, characters 0-4"
      }

      * showMessage notifications:
      ocamlformat: syntax error on line 3, characters 0-4 |}]

let%expect_test "format code with a misplaced doc comment" =
  test
    ~dot_ocamlformat
    ~src:(String.trim {|
let foo =
  let bar = 1 in

  (** *)

  bar |})
    ();
  [%expect
    {|
      * response:
      jsonrpc response error {
        "code": -32603,
        "message": "ocamlformat: warning 50 (unexpected-docstring) on line 4, characters 2-8: unattached documentation comment (ignored). Hint: (Warning 50) This file contains a documentation comment (** ... *) that the OCaml compiler does not know how to attach to the AST. OCamlformat does not support these cases. You can find more information at: https://github.com/ocaml-ppx/ocamlformat#overview. If you'd like to disable this check and let ocamlformat make a choice (though it might not be consistent with the ocaml compilers and odoc), you can set the --no-comment-check option."
      }

      * showMessage notifications:
      ocamlformat: warning 50 (unexpected-docstring) on line 4, characters 2-8: unattached documentation comment (ignored). Hint: (Warning 50) This file contains a documentation comment (** ... *) that the OCaml compiler does not know how to attach to the AST. OCamlformat does not support these cases. You can find more information at: https://github.com/ocaml-ppx/ocamlformat#overview. If you'd like to disable this check and let ocamlformat make a choice (though it might not be consistent with the ocaml compilers and odoc), you can set the --no-comment-check option. |}]

let%expect_test "format code with one syntax error and a misplaced comment" =
  test ~dot_ocamlformat ~src:{|
let bl = 1

(** *)

let foo
        bar|} ();
  [%expect
    {|
      * response:
      jsonrpc response error {
        "code": -32603,
        "message": "ocamlformat: syntax error on line 6, characters 11-11"
      }

      * showMessage notifications:
      ocamlformat: syntax error on line 6, characters 11-11 |}]

let%expect_test "format code with one syntax error" =
  test ~dot_ocamlformat ~src:{|
let foo =

module M = struct end|} ();
  [%expect
    {|
      * response:
      jsonrpc response error {
        "code": -32603,
        "message": "ocamlformat: syntax error on line 3, characters 0-6"
      }

      * showMessage notifications:
      ocamlformat: syntax error on line 3, characters 0-6 |}]
