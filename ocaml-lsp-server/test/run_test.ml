let _ =
  let sh, flag = if Sys.win32 then ("cmd", "/C") else ("sh", "-c") in
  let cmd =
    Bos.Cmd.(v sh % flag % "yarn --cwd ocaml-lsp-server/test/e2e test")
  in
  let result = Bos.OS.Cmd.(run_out ~err:err_run_out cmd |> to_stdout) in
  match result with
  | Ok _ -> ()
  | Error (`Msg e) ->
    Printf.eprintf "[Error]: %s\n" e;
    exit 1
