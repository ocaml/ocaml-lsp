let run () = Ocaml_lsp_server.run ()

let () =
  let open Cmdliner in
  Printexc.record_backtrace true;

  let cmd =
    let doc = "Start OCaml LSP server (only stdio transport is supported)" in
    let version = Version.get () in
    ( Term.(const run $ pure ())
    , Term.info "ocamllsp" ~version ~doc ~exits:Term.default_exits )
  in

  Term.(exit @@ eval cmd)
