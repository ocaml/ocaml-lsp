open Import

type t = Uri.t

let equal = Uri.equal

let hash x = String.hash (Uri.to_string x)

let t_of_yojson yj = Uri.of_string (string_of_yojson yj)

let yojson_of_t uri = yojson_of_string (Uri.to_string uri)

let to_string = Uri.to_string

let to_path (uri : t) =
  let authority =
    Option.map (Uri.host uri) ~f:(fun host ->
        Printf.sprintf "%s%s%s"
          ( match Uri.userinfo uri with
          | Some u -> u ^ "@"
          | None -> "" )
          host
          ( match Uri.port uri with
          | Some p -> string_of_int p ^ ":"
          | None -> "" ))
  in
  let has_authority = Option.is_some authority in
  let scheme = Option.value (Uri.scheme uri) ~default:"" in
  let path = Uri.path uri in
  let is_letter char =
    let code = Char.code char in
    (code >= Char.code 'A' && code <= Char.code 'Z')
    || (code >= Char.code 'a' && code <= Char.code 'z')
  in
  let value =
    if has_authority && String.length path > 1 && String.equal scheme "file"
    then
      Printf.sprintf "//%s%s" (Option.value_exn authority) path
    else if path.[0] == '/' && is_letter path.[1] && path.[2] == ':' then
      String.sub path ~pos:1 ~len:(String.length path - 1)
    else
      path
  in
  if Sys.win32 then
    String.replace_all ~pattern:"/" ~with_:"\\" value
  else
    value

let parse_authority auth =
  if Int.equal (String.length auth) 0 then
    (None, None, None)
  else
    let reg = Str.regexp {|\(\([^@]*\)@\)?\([^:@]+\)\(:\([0-9]+\)\)?|} in
    let matched = Str.string_match reg auth 0 in
    if not matched then raise (Invalid_argument "Uri.parse_authority");
    let userinfo = Option.try_with (fun () -> Str.matched_group 2 auth) in
    let host = Str.matched_group 3 auth in
    let port =
      Option.try_with (fun () -> Int.of_string_exn (Str.matched_group 5 auth))
    in
    (userinfo, Some host, port)

let of_path (path : string) =
  let path =
    if Sys.win32 then
      String.replace_all ~pattern:"\\" ~with_:"/" path
    else
      path
  in
  let authority, path =
    if path.[0] == '/' && path.[1] == '/' then
      match String.index_from_opt path 2 '/' with
      | None -> (String.drop path 2, "/")
      | Some idx ->
        let act_path = String.drop path idx in
        let act_path =
          if Int.equal (String.length act_path) 0 then
            "/"
          else
            path
        in
        (String.sub ~pos:2 ~len:(idx - 2) path, act_path)
    else
      ("", path)
  in
  let userinfo, host, port = parse_authority authority in
  Uri.make ~scheme:"file" ?userinfo ?host ?port ~path ()

let pp = Uri.pp

let to_dyn x = Dyn.Encoder.string (Uri.to_string x)
