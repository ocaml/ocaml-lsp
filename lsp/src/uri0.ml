(* This module is based on the [vscode-uri] implementation:
   https://github.com/microsoft/vscode-uri/blob/main/src/uri.ts. It only
   supports scheme, authority and path. Query, port and fragment are not
   implemented *)

open Import

module Private = struct
  let win32 = ref Sys.win32
end

type t =
  { scheme : string
  ; authority : string
  ; path : string
  }

let backslash_to_slash =
  String.map ~f:(function
    | '\\' -> '/'
    | _ as c -> c)

let slash_to_backslash =
  String.map ~f:(function
    | '/' -> '\\'
    | _ as c -> c)

let of_path path =
  let path = if !Private.win32 then backslash_to_slash path else path in
  let path, authority =
    let len = String.length path in
    if len = 0 then ("/", "")
    else if len > 1 && path.[0] = '/' && path.[1] = '/' then
      let offset = 2 in
      let idx = String.index_from_opt path offset '/' in
      match idx with
      | None -> ("/", String.sub path ~pos:offset ~len:(len - offset))
      | Some i ->
        let authority = String.sub path ~pos:offset ~len:(i - offset) in
        let path =
          let path = String.sub path ~pos:i ~len:(len - i) in
          if String.is_empty path then "/" else path
        in
        (path, authority)
    else (path, "")
  in
  let path = if path.[0] <> '/' then "/" ^ path else path in
  { scheme = "file"; authority; path }

let to_path { path; authority; scheme } =
  let path =
    let len = String.length path in
    if len = 0 then "/"
    else
      let buff = Buffer.create 64 in
      (if (not (String.is_empty authority)) && len > 1 && scheme = "file" then (
       Buffer.add_string buff "//";
       Buffer.add_string buff authority;
       Buffer.add_string buff path)
      else if len < 3 then Buffer.add_string buff path
      else
        let c0 = path.[0] in
        let c1 = path.[1] in
        let c2 = path.[2] in
        if
          c0 = '/'
          && ((c1 >= 'A' && c1 <= 'Z') || (c1 >= 'a' && c1 <= 'z'))
          && c2 = ':'
        then (
          Buffer.add_char buff (Char.lowercase_ascii c1);
          Buffer.add_string buff
            (String.sub path ~pos:2 ~len:(String.length path - 2)))
        else Buffer.add_string buff path);
      Buffer.contents buff
  in
  if !Private.win32 then slash_to_backslash path else path

let of_string s =
  let Uri_lexer.{ scheme; authority; path } = Uri_lexer.of_string s in
  let scheme = scheme |> Option.value ~default:"file" in
  let authority =
    authority |> Option.map Uri.pct_decode |> Option.value ~default:""
  in
  let path =
    let path = path |> Uri.pct_decode in
    match scheme with
    | "http" | "https" | "file" ->
      if String.is_empty path then "/"
      else if path.[0] <> '/' then "/" ^ path
      else path
    | _ -> path
  in
  { scheme; authority; path }

let encode ?(allow_slash = false) s =
  let allowed_chars = if allow_slash then "/" else "" in
  Uri.pct_encode ~component:(`Custom (`Generic, allowed_chars, "")) s

let to_string { scheme; authority; path } =
  let buff = Buffer.create 64 in

  if not (String.is_empty scheme) then (
    Buffer.add_string buff scheme;
    Buffer.add_char buff ':');

  if authority = "file" || scheme = "file" then Buffer.add_string buff "//";

  (*TODO: implement full logic:
    https://github.com/microsoft/vscode-uri/blob/96acdc0be5f9d5f2640e1c1f6733bbf51ec95177/src/uri.ts#L605 *)
  (if not (String.is_empty authority) then
   let s = String.lowercase_ascii authority in
   Buffer.add_string buff (encode s));

  (if not (String.is_empty path) then
   let encode = encode ~allow_slash:true in
   let colon = "%3A" in
   let len = String.length path in
   if len >= 3 && path.[0] = '/' && path.[2] = ':' then (
     let drive_letter = Char.lowercase_ascii path.[1] in
     if drive_letter >= 'a' && drive_letter <= 'z' then (
       Buffer.add_char buff '/';
       Buffer.add_char buff drive_letter;
       Buffer.add_string buff colon;
       let s = String.sub path ~pos:3 ~len:(len - 3) |> encode in
       Buffer.add_string buff s))
   else if len >= 2 && path.[1] = ':' then (
     let drive_letter = Char.lowercase_ascii path.[0] in
     if drive_letter >= 'a' && drive_letter <= 'z' then (
       Buffer.add_char buff drive_letter;
       Buffer.add_string buff colon;
       let s = String.sub path ~pos:2 ~len:(len - 2) |> encode in
       Buffer.add_string buff s))
   else Buffer.add_string buff (encode path));

  Buffer.contents buff

let yojson_of_t t = `String (to_string t)

let t_of_yojson json = Json.Conv.string_of_yojson json |> of_string

let equal = ( = )

let compare (x : t) (y : t) = Stdlib.compare x y

let hash = Hashtbl.hash

let to_dyn { scheme; authority; path } =
  let open Dyn in
  record
    [ ("scheme", string scheme)
    ; ("authority", string authority)
    ; ("path", string path)
    ]
