let run log_file delay = Ocaml_lsp_server.run ~log_file ~delay

let () =
  let open Cmdliner in
  Printexc.record_backtrace true;

  let log_file =
    let open Arg in
    let doc = "Enable logging to file (pass `-' for logging to stderr)" in
    let env = env_var "OCAML_LSP_SERVER_LOG" in
    value & opt (some string) None & info [ "log-file" ] ~docv:"FILE" ~doc ~env
  in

  let delay =
    let open Arg in
    let doc = "Delay between updating a file and receiving diagnostics" in
    value & opt float 0.75 & info [ "delay" ] ~docv:"FILE" ~doc
  in

  let cmd =
    let doc = "Start OCaml LSP server (only stdio transport is supported)" in
    ( Term.(const run $ log_file $ delay)
    , Term.info "ocamllsp" ~doc ~exits:Term.default_exits )
  in

  Term.(exit @@ eval cmd)
