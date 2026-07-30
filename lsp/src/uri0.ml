(* This module is based on the [vscode-uri] implementation:
   https://github.com/microsoft/vscode-uri/blob/main/src/uri.ts. *)

open Import

module Private = struct
  let win32 = ref Sys.win32
end

type t = Uri_lexer.t =
  { scheme : string option
  ; authority : string option
  ; path : string
  ; query : string option
  ; fragment : string option
  }

let query t = Option.map Uri_lexer.decode t.query
let fragment t = Option.map Uri_lexer.decode t.fragment

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

let is_drive_letter = function
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false
;;

let lowercase_drive path =
  let len = String.length path in
  if len >= 3 && path.[0] = '/' && is_drive_letter path.[1] && path.[2] = ':'
  then
    "/"
    ^ String.make 1 (Char.lowercase_ascii path.[1])
    ^ String.sub path ~pos:2 ~len:(len - 2)
  else path
;;

let decoded_path { path; scheme; _ } =
  let path = Uri_lexer.decode path in
  match scheme with
  | None | Some ("http" | "https" | "file") ->
    String.add_prefix_if_not_exists path ~prefix:"/"
  | Some _ -> path
;;

let to_path ({ authority; scheme; _ } as t) =
  let path = decoded_path t in
  let authority = Option.value ~default:"" authority |> Uri_lexer.decode in
  let scheme = Option.value ~default:"file" scheme in
  let len = String.length path in
  let path =
    if len = 0
    then "/"
    else if (not (String.is_empty authority)) && len > 1 && scheme = "file"
    then "//" ^ authority ^ path
    else if
      len >= 3 && path.[0] = '/' && is_drive_letter path.[1] && Char.equal path.[2] ':'
    then
      String.make 1 (Char.lowercase_ascii path.[1]) ^ String.sub path ~pos:2 ~len:(len - 2)
    else path
  in
  if !Private.win32 then slash_to_backslash path else path
;;

let encode ?(allow_slash = false) s =
  let len = String.length s in
  let buf = Buffer.create len in
  let rec scan start cur =
    if cur >= len
    then Buffer.add_substring buf s start (cur - start)
    else (
      let c = s.[cur] in
      if (allow_slash && c = '/') || Uri_lexer.is_unreserved c
      then scan start (cur + 1)
      else (
        if cur > start then Buffer.add_substring buf s start (cur - start);
        Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code c));
        scan (cur + 1) (cur + 1)))
  in
  scan 0 0;
  Buffer.contents buf
;;

(* Both URI syntax and filesystem paths go through the same file normalization.
   Inputs here are decoded; the result is canonical encoded components. *)
let normalize_file ~authority ~path =
  let authority = String.lowercase_ascii authority in
  let path = String.add_prefix_if_not_exists path ~prefix:"/" in
  let path = if String.is_empty authority then lowercase_drive path else path in
  encode authority, encode ~allow_slash:true path
;;

let of_string source =
  let { scheme; authority; path; query; fragment } = Uri_lexer.of_string source in
  let normalize = Uri_lexer.decode ~only_unreserved:true in
  let scheme = Option.map String.lowercase_ascii scheme in
  let authority, path =
    match scheme with
    | Some "file" ->
      let authority, path =
        normalize_file
          ~authority:(Option.value ~default:"" authority |> Uri_lexer.decode)
          ~path:(Uri_lexer.decode path)
      in
      Some authority, path
    | None | Some _ -> Option.map normalize authority, normalize path
  in
  { scheme
  ; authority
  ; path
  ; query = Option.map normalize query
  ; fragment = Option.map normalize fragment
  }
;;

let of_path path =
  let path = if !Private.win32 then backslash_to_slash path else path in
  let authority, path = Uri_lexer.of_path path in
  let authority, path = normalize_file ~authority ~path in
  { scheme = Some "file"
  ; authority = Some authority
  ; path
  ; query = None
  ; fragment = None
  }
;;

let to_string { scheme; authority; path; query; fragment } =
  let buf = Buffer.create 64 in
  Option.iter
    (fun scheme ->
       Buffer.add_string buf scheme;
       Buffer.add_char buf ':')
    scheme;
  Option.iter
    (fun authority ->
       Buffer.add_string buf "//";
       Buffer.add_string buf authority)
    authority;
  Buffer.add_string buf path;
  Option.iter
    (fun query ->
       Buffer.add_char buf '?';
       Buffer.add_string buf query)
    query;
  Option.iter
    (fun fragment ->
       Buffer.add_char buf '#';
       Buffer.add_string buf fragment)
    fragment;
  Buffer.contents buf
;;

let yojson_of_t t = `String (to_string t)
let t_of_yojson json = Json.Conv.string_of_yojson json |> of_string
let compare : t -> t -> int = Stdlib.compare
let equal : t -> t -> bool = ( = )
let hash : t -> int = Hashtbl.hash
