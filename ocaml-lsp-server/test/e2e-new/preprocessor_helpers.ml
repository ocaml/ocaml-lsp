open Test.Import

type project =
  { path : string
  ; uri : DocumentUri.t
  }

let setup ~name ~fixture ~dune_file =
  let project_root = Sys.getenv "DUNE_PROJECT_ROOT" in
  let dir = Test.temp_dir ~temp_dir:project_root ("ocamllsp-" ^ name ^ "-") in
  let path = Filename.concat dir (name ^ ".ml") in
  let fixture = Filename.concat project_root fixture in
  Test.write_file path (Io.String_path.read_file fixture);
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.24)\n";
  Test.write_file (Filename.concat dir "dune") dune_file;
  Test.run_command ~cwd:dir "dune build";
  { path; uri = DocumentUri.of_path path }
;;

let hover ?(prep = fun _ -> Fiber.return ()) ~handler ~project ~position ~capture () =
  Test.run_initialized ~handler
  @@ fun client ->
  let source = Io.String_path.read_file project.path in
  let* () = Test.open_document ~client ~uri:project.uri ~source () in
  let* () = prep client in
  let* () =
    let+ response = Hover_helpers.hover ~uri:project.uri client position in
    Hover_helpers.print_hover response
  in
  let output = capture () in
  let+ () = Test.shutdown_client client in
  output
;;
