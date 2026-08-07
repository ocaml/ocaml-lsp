open Test.Import

type project =
  { path : string
  ; uri : DocumentUri.t
  }

let setup ~name ~fixture ~dune_file =
  let project_root = Sys.getenv "DUNE_PROJECT_ROOT" in
  (* Create the temp project outside the checkout: dune refuses to treat a
     directory under a hidden path (e.g. [.scratch]) as a project root, which
     breaks both [dune build] and the server's [dune ocaml-merlin] config
     discovery. *)
  let dir = Test.temp_dir ("ocamllsp-" ^ name ^ "-") in
  let path = Filename.concat dir (name ^ ".ml") in
  let fixture = Filename.concat project_root fixture in
  Test.write_file path (Fs_io.read_file fixture |> Result.ok_exn);
  Test.write_file (Filename.concat dir "dune-project") "(lang dune 3.24)\n";
  Test.write_file (Filename.concat dir "dune") dune_file;
  Test.run_command ~cwd:dir "dune build --root . @check";
  { path; uri = DocumentUri.of_path path }
;;

let hover ?(prep = fun _ -> Fiber.return ()) ~handler ~project ~position ~capture () =
  Test.run_initialized ~handler
  @@ fun client ->
  let source = Fs_io.read_file project.path |> Result.ok_exn in
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
