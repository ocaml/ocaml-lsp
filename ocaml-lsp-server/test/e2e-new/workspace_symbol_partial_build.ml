open Test.Import
open Workspace_symbol_test_helpers

let%expect_test "missing build directories return empty results without notifications" =
  let workspace_a, workspace_b = setup_workspaces () in
  clean_project workspace_a;
  clean_project workspace_b;
  let show_messages = Queue.create () in
  let on_notification _ = function
    | Lsp.Server_notification.ShowMessage message ->
      Queue.push show_messages message;
      Fiber.return ()
    | _ -> Fiber.return ()
  in
  let workspaces = [ workspace_a; workspace_b ] in
  let print_response label = function
    | None -> Printf.printf "%s: null\n" label
    | Some symbols ->
      Printf.printf "%s: " label;
      symbols
      |> List.map ~f:(fun symbol -> `String (to_test_result workspaces symbol))
      |> fun symbols -> Test.print_result (`List symbols)
  in
  run ~on_notification workspaces (fun client ->
    let* first = workspace_symbol client "" in
    let* second = workspace_symbol client "changed query" in
    print_response "first result" first;
    print_response "second result" second;
    Fiber.return ());
  Printf.printf "show messages: ";
  let messages =
    show_messages |> Queue.to_list |> List.map ~f:ShowMessageParams.yojson_of_t
  in
  Test.print_result (`List messages);
  [%expect
    {|
    first result: []
    second result: []
    show messages: []
    |}]
;;

let%expect_test "mixed workspaces return symbols only from built workspaces" =
  let workspace_a, workspace_b = setup_workspaces () in
  build_project workspace_a;
  clean_project workspace_b;
  let show_messages = Queue.create () in
  let on_notification _ = function
    | Lsp.Server_notification.ShowMessage message ->
      Queue.push show_messages message;
      Fiber.return ()
    | _ -> Fiber.return ()
  in
  let workspaces = [ workspace_b; workspace_a ] in
  run ~on_notification workspaces (fun client ->
    let* symbols = workspace_symbol client "a_x" in
    print_symbols workspaces symbols;
    Fiber.return ());
  Printf.printf "show messages: ";
  let messages =
    show_messages |> Queue.to_list |> List.map ~f:ShowMessageParams.yojson_of_t
  in
  Test.print_result (`List messages);
  [%expect
    {|
    a_x 12 /workspace_symbol_A/bin/a.ml 0:0 0:11
    show messages: []
    |}]
;;

let setup_generated_workspace () =
  let path = Test.temp_dir "ocamllsp-generated-workspace-symbol-" in
  let lib = Stdlib.Filename.concat path "lib" in
  mkdir lib;
  Test.write_file (Stdlib.Filename.concat path "dune-project") "(lang dune 2.5)\n";
  Test.write_file
    (Stdlib.Filename.concat lib "dune")
    {dune|
(library
 (name generated_source))

(rule
 (target gen.ml)
 (action
  (with-stdout-to
   %{target}
   (echo "let generated_workspace_symbol = 42"))))
|dune};
  Test.run_command ~cwd:path "dune build";
  let name = "generated-source" in
  let uri = DocumentUri.of_path path in
  { name; path; folder = WorkspaceFolder.create ~name ~uri }
;;

let relative_path ~root path =
  let prefix = root ^ Stdlib.Filename.dir_sep in
  if Stdlib.String.starts_with ~prefix path
  then String.drop path (String.length prefix)
  else path
;;

let%expect_test "generated source has an existing workspace-symbol location" =
  let workspace = setup_generated_workspace () in
  run [ workspace ] (fun client ->
    let* symbols = workspace_symbol client "generated_workspace_symbol" in
    let symbols = Option.value symbols ~default:[] in
    (match
       List.find_map symbols ~f:(fun (symbol : SymbolInformation.t) ->
         if String.equal symbol.name "generated_workspace_symbol"
         then Some symbol
         else None)
     with
     | None -> print_endline "generated_workspace_symbol: not found"
     | Some symbol ->
       let path = DocumentUri.to_path symbol.location.uri in
       Printf.printf "path: %s\n" (relative_path ~root:workspace.path path);
       let contents = Io.String_path.read_file path in
       Printf.printf "contents: %s\n" (Yojson.Safe.to_string (`String contents)));
    Fiber.return ());
  [%expect
    {|
    path: _build/default/lib/gen.ml
    contents: "let generated_workspace_symbol = 42"
    |}]
;;
