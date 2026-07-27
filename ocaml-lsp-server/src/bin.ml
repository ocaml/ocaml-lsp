open Import

let path_separator = if Sys.win32 then ';' else ':'

let path =
  lazy
    (Option.value ~default:"" (Sys.getenv_opt "PATH")
     |> String.split ~on:path_separator
     |> List.filter ~f:(fun path -> not (String.is_empty path)))
;;

let add_executable_suffix program =
  if Sys.win32 && not (String.is_suffix (String.lowercase program) ~suffix:".exe")
  then program ^ ".exe"
  else program
;;

let exists path =
  match Unix.stat path with
  | { st_kind = S_DIR; _ } -> false
  | exception Unix.Unix_error _ -> false
  | _ -> true
;;

let which program =
  let program = add_executable_suffix program in
  List.find_map (Lazy.force path) ~f:(fun directory ->
    let path = Filename.concat directory program in
    Option.some_if (exists path) path)
;;
