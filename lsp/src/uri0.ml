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
      let idx = String.index_from_opt path 2 '/' in
      match idx with
      | None -> ("/", String.sub path ~pos:2 ~len:(len - 2))
      | Some i ->
        let authority = String.sub path ~pos:2 ~len:(i - 2) in
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
    else if (not (String.is_empty authority)) && len > 1 && scheme = "file" then
      "//" ^ authority ^ path
    else if len < 3 then path
    else
      let c0 = path.[0] in
      let c1 = path.[1] in
      let c2 = path.[2] in
      if
        c0 = '/'
        && ((c1 >= 'A' && c1 <= 'Z') || (c1 >= 'a' && c1 <= 'z'))
        && c2 = ':'
      then
        String.make 1 (Char.lowercase_ascii c1)
        ^ String.sub path ~pos:2 ~len:(String.length path - 2)
      else path
  in
  if !Private.win32 then slash_to_backslash path else path

let of_string s =
  let re =
    Re.Perl.re "^(([^:/?#]+?):)?(\\/\\/([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
    |> Re.compile
  in
  let res = Re.exec re s in
  let scheme = Re.Group.get_opt res 2 |> Option.value ~default:"file" in
  let group re n = Re.Group.get_opt re n |> Option.value ~default:"" in
  let authority = group res 4 |> Uri.pct_decode in
  let path =
    let path = group res 5 |> Uri.pct_decode in
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
  let res = ref "" in

  if not (String.is_empty scheme) then res := scheme ^ ":";

  if authority = "file" || scheme = "file" then res := !res ^ "//";

  (*TODO: implement full logic:
    https://github.com/microsoft/vscode-uri/blob/96acdc0be5f9d5f2640e1c1f6733bbf51ec95177/src/uri.ts#L605 *)
  (if not (String.is_empty authority) then
   let value = String.lowercase_ascii authority in
   res := !res ^ encode value);

  if not (String.is_empty path) then (
    let value = ref path in
    let len = String.length path in
    (*TODO: should we use charCode instead ? *)
    (if len >= 3 && path.[0] = '/' && path.[2] = ':' then (
     let code = path.[1] in
     if code >= 'A' && code <= 'Z' then
       value :=
         "/"
         ^ (String.make 1 code |> String.lowercase_ascii)
         ^ ":"
         ^ String.sub path ~pos:3 ~len:(len - 3))
    else if len >= 2 && path.[1] = ':' then
      let code = path.[0] in
      if code >= 'A' && code <= 'Z' then
        value :=
          (String.make 1 code |> String.lowercase_ascii)
          ^ ":"
          ^ String.sub path ~pos:2 ~len:(len - 2));
    res := !res ^ encode ~allow_slash:true !value);

  !res

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
