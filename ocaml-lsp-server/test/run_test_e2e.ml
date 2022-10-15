let () =
  let yarn =
    [ "ocaml-lsp-server"; "test"; "e2e"; "node_modules"; "yarn"; "bin"; "yarn" ]
  in
  let cmd =
    Filename.quote_command
      (String.concat Filename.dir_sep yarn)
      [ "--cwd"; "ocaml-lsp-server/test/e2e"; "test" ]
  in
  Sys.command cmd |> exit
