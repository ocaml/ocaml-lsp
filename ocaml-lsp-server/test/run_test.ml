let _ =
  let () = Sys.chdir Fpath.(v "ocaml-lsp-server/test/e2e" |> to_string) in
  let sh, flag = if Sys.win32 then ("cmd", "/C") else ("sh", "-c") in
  let cmd =
    Bos.Cmd.(v sh % flag % Fpath.(v "./node_modules/.bin/jest" |> to_string))
  in
  let result = Bos.OS.Cmd.(run_out ~err:err_run_out cmd |> to_stdout) in
  match result with
  | Ok _ -> ()
  | Error (`Msg e) ->
    Printf.eprintf "[Error]: %s" e;
    exit 1
