(* This module is based on the [vscode-uri] implementation:
   https://github.com/microsoft/vscode-uri/blob/main/src/uri.ts. It only
   supports scheme, authority and path. Query, port and fragment are not
   implemented *)

open Import

module Private = struct
  let win32 = ref Sys.win32
end

type t = Uri_lexer.t =
  { scheme : string
  ; authority : string
  ; path : string
  }

let backslash_to_slash =
  String.map ~f:(function
    | '\\' -> '/'
    | c -> c)

let slash_to_backslash =
  String.map ~f:(function
    | '/' -> '\\'
    | c -> c)

let of_path path =
  let path = if !Private.win32 then backslash_to_slash path else path in
  Uri_lexer.of_path path

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let to_path { path; authority; scheme } =
  let path =
    let len = String.length path in
    if len = 0 then "/"
    else
      let buff = Buffer.create 64 in
      (if (not (String.is_empty authority)) && len > 1 && scheme = "file" then
       Buffer.add_string buff (Printf.sprintf "//%s%s" authority path)
      else if len < 3 then Buffer.add_string buff path
      else
        let c0 = path.[0] in
        let c1 = path.[1] in
        let c2 = path.[2] in
        if c0 = '/' && is_letter c1 && c2 = ':' then (
          Buffer.add_char buff (Char.lowercase_ascii c1);
          Buffer.add_substring buff path 2 (String.length path - 2))
        else Buffer.add_string buff path);
      Buffer.contents buff
  in
  if !Private.win32 then slash_to_backslash path else path

let of_string = Uri_lexer.of_string

let encode ?(allow_slash = false) s =
  let allowed_chars = if allow_slash then "/" else "" in
  Uri.pct_encode ~component:(`Custom (`Generic, allowed_chars, "")) s

let to_string { scheme; authority; path } =
  let buff = Buffer.create 64 in

  if not (String.is_empty scheme) then
    Buffer.add_string buff (Printf.sprintf "%s:" scheme);

  if authority = "file" || scheme = "file" then Buffer.add_string buff "//";

  (*TODO: implement full logic:
    https://github.com/microsoft/vscode-uri/blob/96acdc0be5f9d5f2640e1c1f6733bbf51ec95177/src/uri.ts#L605 *)
  (if not (String.is_empty authority) then
   let s = String.lowercase_ascii authority in
   Buffer.add_string buff (encode s));

  (if not (String.is_empty path) then
   let encode = encode ~allow_slash:true in
   let encoded_colon = "%3A" in
   let len = String.length path in
   if len >= 3 && path.[0] = '/' && path.[2] = ':' then (
     let drive_letter = path.[1] in
     if is_letter drive_letter then
       let drive_letter = Char.lowercase_ascii drive_letter in
       let s = encode (String.sub path ~pos:3 ~len:(len - 3)) in
       Buffer.add_string buff
         (Printf.sprintf "/%c%s%s" drive_letter encoded_colon s))
   else if len >= 2 && path.[1] = ':' then (
     let drive_letter = path.[0] in
     if is_letter drive_letter then
       let drive_letter = Char.lowercase_ascii drive_letter in
       let s = encode (String.sub path ~pos:2 ~len:(len - 2)) in
       Buffer.add_string buff
         (Printf.sprintf "%c%s%s" drive_letter encoded_colon s))
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
