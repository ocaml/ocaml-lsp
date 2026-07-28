open Test.Import

let options = FormattingOptions.create ~tabSize:2 ~insertSpaces:true ()

let request client uri line =
  let textDocument = TextDocumentIdentifier.create ~uri in
  let position = Position.create ~line ~character:0 in
  Client.request
    client
    (Lsp.Client_request.TextDocumentOnTypeFormatting
       (DocumentOnTypeFormattingParams.create ~textDocument ~position ~ch:"\n" ~options))
;;

let print_edits = function
  | None -> print_endline "on-type formatting unavailable"
  | Some edits -> `List (List.map edits ~f:TextEdit.yojson_of_t) |> Test.print_result
;;

let%expect_test "indents after a newline and reuses the RPC process" =
  let rpc = Bin.which "ocp-indent-rpc" |> Option.value_exn in
  let dir = Test.temp_dir "ocamllsp-on-type-formatting-" in
  let bin_dir = Filename.concat dir "bin" in
  let log = Filename.concat dir "launches" in
  Unix.mkdir bin_dir 0o700;
  let wrapper = Filename.concat bin_dir "ocp-indent-rpc" in
  Test.write_file
    wrapper
    (Printf.sprintf
       "#!/bin/sh\nprintf 'launch\\n' >> %s\nexec %s\n"
       (Filename.quote log)
       (Filename.quote rpc));
  Unix.chmod wrapper 0o700;
  let path = Filename.concat dir "test.ml" in
  let uri = DocumentUri.of_path path in
  let source = "let f =\n\n" in
  let textDocument =
    TextDocumentItem.create
      ~uri
      ~languageId:(LanguageKind.Other "ocaml")
      ~version:0
      ~text:source
  in
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  let search_path =
    let path = Sys.getenv_opt "PATH" |> Option.value ~default:"" in
    bin_dir ^ ":" ^ path
  in
  Test.run_initialized
    ~handler
    ~extra_env:[ "PATH=" ^ search_path ]
    (fun client ->
       let* () =
         Client.notification
           client
           (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
       in
       assert (not (Sys.file_exists log));
       let* edits = request client uri 1 in
       print_edits edits;
       let* (_ : TextEdit.t list option) = request client uri 1 in
       let launches =
         Fs_io.read_file log |> Result.ok_exn |> String.split_lines |> List.length
       in
       Printf.printf "RPC launches: %d\n" launches;
       let* () = Client.request client Shutdown in
       let* () = Fiber.Ivar.read diagnostics in
       Client.notification client Exit);
  [%expect
    {|
    [
      {
        "newText": "  ",
        "range": {
          "end": { "character": 0, "line": 1 },
          "start": { "character": 0, "line": 1 }
        }
      }
    ]
    RPC launches: 1
    |}]
;;
