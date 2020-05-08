let run log_file _refmt_width = Ocaml_lsp_server.run ~log_file

let default_refmt_width = 80

let () =
  let open Cmdliner in
  Printexc.record_backtrace true;

  let log_file =
    let open Arg in
    let doc = "Enable logging to file (pass `-' for logging to stderr)" in
    let env = env_var "OCAML_LSP_SERVER_LOG" in
    value & opt (some string) None & info [ "log-file" ] ~docv:"FILE" ~doc ~env
  in

  let refmt_width =
    let open Arg in
    let doc = "refmt wrapping width" in
    value & opt int default_refmt_width & info ["w"; "refmt-width"] ~docv:"WIDTH" ~doc
  in

  let cmd =
    let doc = "Start OCaml LSP server (only stdio transport is supported)" in
    ( Term.(const run $ log_file $ refmt_width)
    , Term.info "ocamllsp" ~doc ~exits:Term.default_exits )
  in

  Term.(exit @@ eval cmd)
