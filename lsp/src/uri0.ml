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
  ; query : string option
  }

let query t = t.query

let backslash_to_slash =
  String.map ~f:(function
    | '\\' -> '/'
    | c -> c)
;;

let slash_to_backslash =
  String.map ~f:(function
    | '/' -> '\\'
    | c -> c)
;;

let of_path path =
  let path = if !Private.win32 then backslash_to_slash path else path in
  Uri_lexer.of_path path
;;

let to_path { path; authority; scheme; query } =
  let path =
    let len = String.length path in
    if len = 0
    then "/"
    else (
      let buff = Buffer.create 64 in
      if (not (String.is_empty authority)) && len > 1 && scheme = "file"
      then (
        Buffer.add_string buff "//";
        Buffer.add_string buff authority;
        Buffer.add_string buff path)
      else if len < 3
      then Buffer.add_string buff path
      else (
        let c0 = path.[0] in
        let c1 = path.[1] in
        let c2 = path.[2] in
        if c0 = '/' && ((c1 >= 'A' && c1 <= 'Z') || (c1 >= 'a' && c1 <= 'z')) && c2 = ':'
        then (
          Buffer.add_char buff (Char.lowercase_ascii c1);
          Buffer.add_substring buff path 2 (String.length path - 2))
        else Buffer.add_string buff path);
      (match query with
       | None -> ()
       | Some query ->
         Buffer.add_char buff '?';
         Buffer.add_string buff query);
      Buffer.contents buff)
  in
  if !Private.win32 then slash_to_backslash path else path
;;

let of_string = Uri_lexer.of_string

let safe_chars =
  let a = Array.make 256 false in
  let always_safe =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_.-~"
  in
  for i = 0 to String.length always_safe - 1 do
    let c = Char.code always_safe.[i] in
    a.(c) <- true
  done;
  a
;;

let slash_code = 47

(* https://github.com/mirage/ocaml-uri/blob/master/lib/uri.ml#L284 *)
let encode ?(allow_slash = false) s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec scan start cur =
    if cur >= len
    then Buffer.add_substring buf s start (cur - start)
    else (
      let c = Char.code s.[cur] in
      if (allow_slash && c = slash_code) || safe_chars.(c)
      then scan start (cur + 1)
      else (
        if cur > start then Buffer.add_substring buf s start (cur - start);
        Buffer.add_string buf (Printf.sprintf "%%%02X" c);
        scan (cur + 1) (cur + 1)))
  in
  scan 0 0;
  Buffer.contents buf
;;

let to_string { scheme; authority; path; query } =
  let buff = Buffer.create 64 in
  if not (String.is_empty scheme)
  then (
    Buffer.add_string buff scheme;
    Buffer.add_char buff ':');
  if (not (String.is_empty authority)) || scheme = "file" then Buffer.add_string buff "//";
  (*TODO: implement full logic:
    https://github.com/microsoft/vscode-uri/blob/96acdc0be5f9d5f2640e1c1f6733bbf51ec95177/src/uri.ts#L605 *)
  if not (String.is_empty authority)
  then (
    let s = String.lowercase_ascii authority in
    Buffer.add_string buff (encode s));
  if not (String.is_empty path)
  then (
    let encode = encode ~allow_slash:true in
    let encoded_colon = "%3A" in
    let len = String.length path in
    if len >= 3 && path.[0] = '/' && path.[2] = ':'
    then (
      let drive_letter = Char.lowercase_ascii path.[1] in
      if drive_letter >= 'a' && drive_letter <= 'z'
      then (
        Buffer.add_char buff '/';
        Buffer.add_char buff drive_letter;
        Buffer.add_string buff encoded_colon;
        let s = String.sub path ~pos:3 ~len:(len - 3) in
        Buffer.add_string buff (encode s)))
    else if len >= 2 && path.[1] = ':'
    then (
      let drive_letter = Char.lowercase_ascii path.[0] in
      if drive_letter >= 'a' && drive_letter <= 'z'
      then (
        Buffer.add_char buff drive_letter;
        Buffer.add_string buff encoded_colon;
        let s = String.sub path ~pos:2 ~len:(len - 2) in
        Buffer.add_string buff (encode s)))
    else Buffer.add_string buff (encode path));
  (match query with
   | None -> ()
   | Some q ->
     Buffer.add_char buff '?';
     Buffer.add_string buff (encode q));
  Buffer.contents buff
;;

let yojson_of_t t = `String (to_string t)
let t_of_yojson json = Json.Conv.string_of_yojson json |> of_string
let equal = ( = )
let compare (x : t) (y : t) = Stdlib.compare x y
let hash = Hashtbl.hash
