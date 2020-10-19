let rec rm_rf path =
  if Sys.is_directory path then (
    clear path;
    Unix.rmdir path
  ) else
    Unix.unlink path

and clear path =
  Sys.readdir path |> Array.iter (fun name -> rm_rf (Filename.concat path name))

let () = clear "./vendor"
