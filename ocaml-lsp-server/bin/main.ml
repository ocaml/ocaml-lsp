open Stdune
module Cli = Lsp.Cli

let () =
  Printexc.record_backtrace true;
  let version = ref false in
  let prefer_dot_merlin = ref false in
  let arg = Lsp.Cli.Arg.create () in
  let spec =
    [ "--version", Arg.Set version, "print version"
    ; ( "--fallback-read-dot-merlin"
      , Arg.Set prefer_dot_merlin
      , "deprecated, same as --prefer-dot-merlin" )
    ; ( "--prefer-dot-merlin"
      , Arg.Set prefer_dot_merlin
      , "always read Merlin config from existing .merlin files. The `dot-merlin-reader` \
         package must be installed" )
    ]
    @ Cli.Arg.spec arg
  in
  let usage =
    "ocamllsp [ --stdio | --socket PORT | --port PORT | --pipe PIPE ] [ \
     --clientProcessId pid ] [ --prefer-dot-merlin ]"
  in
  Arg.parse spec (fun _ -> raise @@ Arg.Bad "anonymous arguments aren't allowed") usage;
  let channel =
    match Cli.Arg.channel arg with
    | Ok c -> c
    | Error s ->
      Format.eprintf "%s@.%!" s;
      Arg.usage spec usage;
      exit 1
  in
  let version = !version in
  if version
  then (
    let version = Ocaml_lsp_server.Version.get () in
    print_endline version)
  else
    let module Exn_with_backtrace = Stdune.Exn_with_backtrace in
    match
      Exn_with_backtrace.try_with
        (Ocaml_lsp_server.run channel ~prefer_dot_merlin:!prefer_dot_merlin)
    with
    | Ok () -> ()
    | Error exn ->
      Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
      exit 1
;;
