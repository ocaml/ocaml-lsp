let path_sep =
  if Sys.win32 then
    ';'
  else
    ':'

let parse_path ?(sep = path_sep) s =
  String.split s ~on:sep
  |> List.filter_map ~f:(function
       | "" -> None
       | p -> Some (Path.of_filename_relative_to_initial_cwd p))

let cons_path p ~_PATH =
  let p = Path.to_absolute_filename p in
  match _PATH with
  | None -> p
  | Some s -> Printf.sprintf "%s%c%s" p path_sep s

let exe =
  if Sys.win32 then
    ".exe"
  else
    ""

let exists fn =
  match Unix.stat (Path.to_string fn) with
  | { st_kind = S_DIR; _ } -> false
  | exception Unix.Unix_error _ -> false
  | _ -> true

let add_exe prog =
  if String.is_suffix (String.lowercase prog) ~suffix:exe then
    prog
  else
    prog ^ exe

let which ~path prog =
  let prog = add_exe prog in
  List.find_map path ~f:(fun dir ->
      let fn = Path.relative dir prog in
      Option.some_if (exists fn) fn)

let make ~path =
  match which ~path "gmake" with
  | None -> which ~path "make"
  | some -> some
