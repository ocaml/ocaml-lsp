let () =
  Printexc.record_backtrace true;
  let version = ref false in
  Arg.parse
    [ ("--version", Arg.Set version, "print version") ]
    (fun _ -> raise (Arg.Bad "anonymous arguments are not accepted"))
    "ocamllsp";
  let version = !version in
  if version then
    let version = Ocaml_lsp_server.Version.get () in
    print_endline version
  else
    let module Exn_with_backtrace = Stdune.Exn_with_backtrace in
    match Exn_with_backtrace.try_with Ocaml_lsp_server.run with
    | Ok () -> ()
    | Error exn ->
      Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
      exit 1
