open Test.Import
module Req = Ocaml_lsp_server.Custom_request.Switch_impl_intf

let setup_files exts =
  let dir = Test.temp_dir "ocamllsp-switch-" in
  List.iter exts ~f:(fun ext ->
    Test.write_file (Stdlib.Filename.concat dir ("test." ^ ext)) "");
  dir
;;

let uri_of_ext dir ext = DocumentUri.of_path (Stdlib.Filename.concat dir ("test." ^ ext))

let open_document client uri =
  let textDocument =
    TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text:""
  in
  Client.notification
    client
    (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
;;

let switch_impl_intf client uri =
  Test.custom_request client Req.meth (`List [ DocumentUri.yojson_of_t uri ])
;;

let print_response json =
  Yojson.Safe.Util.to_list json
  |> List.map ~f:(fun json -> DocumentUri.t_of_yojson json |> DocumentUri.to_path)
  |> List.map ~f:Stdlib.Filename.basename
  |> List.iter ~f:print_endline
;;

let run_switch_test files_to_create ~from =
  let dir = setup_files files_to_create in
  let on_notification, diagnostics = Test.drain_diagnostics () in
  let handler = Client.Handler.make ~on_notification () in
  Test.run ~handler
  @@ fun client ->
  let run_client () = Test.start_client client in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let uri = uri_of_ext dir from in
    let* () = open_document client uri in
    let* response = switch_impl_intf client uri in
    print_response response;
    let* () = Client.request client Shutdown in
    let* () = Fiber.Ivar.read diagnostics in
    Client.stop client
  in
  Fiber.fork_and_join_unit run_client (fun () -> run)
;;

let%expect_test "test switches (mli => ml)" =
  run_switch_test [ "mli" ] ~from:"mli";
  [%expect {| test.ml |}]
;;

let%expect_test "test switches (mli, ml => ml)" =
  run_switch_test [ "mli"; "ml" ] ~from:"mli";
  [%expect {| test.ml |}]
;;

let%expect_test "test switches (ml => mli)" =
  run_switch_test [ "ml" ] ~from:"ml";
  [%expect {| test.mli |}]
;;

let%expect_test "test switches (ml, mli => mli)" =
  run_switch_test [ "ml"; "mli" ] ~from:"ml";
  [%expect {| test.mli |}]
;;

let%expect_test "test switches (mli, mll => mll)" =
  run_switch_test [ "mli"; "mll" ] ~from:"mli";
  [%expect {| test.mll |}]
;;

let%expect_test "test switches (mli, ml, mll => ml, mll)" =
  run_switch_test [ "mli"; "ml"; "mll" ] ~from:"mli";
  [%expect
    {|
    test.ml
    test.mll |}]
;;

let run_switch_request uri =
  Test.run
  @@ fun client ->
  let run_client () = Test.start_client client in
  let run =
    let* (_ : InitializeResult.t) = Client.initialized client in
    let* response = switch_impl_intf client uri in
    print_response response;
    let* () = Client.request client Shutdown in
    Client.stop client
  in
  Fiber.fork_and_join_unit run_client (fun () -> run)
;;

let%expect_test "can switch from file URI with non-file scheme" =
  let dir = setup_files [ "ml"; "mli" ] in
  let mli_file_uri = uri_of_ext dir "mli" |> DocumentUri.to_string in
  let mli_uri =
    "untitled" ^ Stdlib.String.sub mli_file_uri 4 (Stdlib.String.length mli_file_uri - 4)
    |> DocumentUri.of_string
  in
  run_switch_request mli_uri;
  [%expect {| test.ml |}]
;;
