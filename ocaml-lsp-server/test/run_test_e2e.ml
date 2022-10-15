let () =
  let cmd =
    Filename.quote_command
      "ocaml-lsp-server/test/e2e/node_modules/yarn/bin/yarn"
      [ "--cwd"; "ocaml-lsp-server/test/e2e"; "test" ]
  in
  Sys.command cmd |> exit
