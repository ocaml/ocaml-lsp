(* This module is based on the [vscode-uri] implementation:
   https://github.com/microsoft/vscode-uri/blob/main/src/uri.ts. *)

open Import

module Codec = struct
  let int_of_hex_char = function
    | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
    | 'a' .. 'f' as c -> Some (Char.code c - Char.code 'a' + 10)
    | 'A' .. 'F' as c -> Some (Char.code c - Char.code 'A' + 10)
    | _ -> None
  ;;

  let is_unreserved = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
    | _ -> false
  ;;

  type component =
    | Authority
    | Path
    | Query
    | Fragment

  (* RFC 3986: pchar, plus the delimiters allowed in each component. Brackets
     delimit IP literals in authorities, but must be escaped in paths and suffixes. *)
  let is_allowed_raw component c =
    is_unreserved c
    ||
    match c with
    | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | ':' | '@' -> true
    | '[' | ']' -> component = Authority
    | '/' -> component <> Authority
    | '?' -> component = Query || component = Fragment
    | _ -> false
  ;;

  let add_escape buf c =
    let hex = "0123456789ABCDEF" in
    let byte = Char.code c in
    Buffer.add_char buf '%';
    Buffer.add_char buf hex.[byte lsr 4];
    Buffer.add_char buf hex.[byte land 15]
  ;;

  type mode =
    | Decode
    | Normalize of component

  let convert mode s =
    let len = String.length s in
    let buf = Buffer.create len in
    let rec scan i =
      if i < len
      then (
        let escape =
          if s.[i] = '%' && i + 2 < len
          then (
            match int_of_hex_char s.[i + 1], int_of_hex_char s.[i + 2] with
            | Some high, Some low -> Some (Char.chr ((high lsl 4) + low))
            | _ -> None)
          else None
        in
        match escape with
        | Some c ->
          (match mode with
           | Normalize _ when not (is_unreserved c) -> add_escape buf c
           | Decode | Normalize _ -> Buffer.add_char buf c);
          scan (i + 3)
        | None ->
          let c = s.[i] in
          (match mode with
           | Normalize component when not (is_allowed_raw component c) -> add_escape buf c
           | Decode | Normalize _ -> Buffer.add_char buf c);
          scan (i + 1))
    in
    scan 0;
    Buffer.contents buf
  ;;

  let decode s = if String.contains s '%' then convert Decode s else s

  (* Normalize encoded syntax, not decoded data. In particular, malformed '%' bytes
     must be quoted before decoding their neighbours: %%36%31 becomes %2561, not
     %61 (which would decode a second time as 'a'). *)
  let normalize component s =
    if String.for_all s ~f:(is_allowed_raw component)
    then s
    else convert (Normalize component) s
  ;;

  (* Raw filesystem data: '%' is a filename byte, never an existing escape. *)
  let encode ~allow_slash s =
    let len = String.length s in
    let buf = Buffer.create len in
    let rec scan start cur =
      if cur >= len
      then Buffer.add_substring buf s start (cur - start)
      else (
        let c = s.[cur] in
        if (allow_slash && c = '/') || is_unreserved c
        then scan start (cur + 1)
        else (
          if cur > start then Buffer.add_substring buf s start (cur - start);
          add_escape buf c;
          scan (cur + 1) (cur + 1)))
    in
    scan 0 0;
    Buffer.contents buf
  ;;
end

let win32 = ref Sys.win32

module For_tests = struct
  let with_win32 value f =
    let previous = !win32 in
    Fun.protect
      ~finally:(fun () -> win32 := previous)
      (fun () ->
         win32 := value;
         f ())
  ;;
end

type t = Uri_lexer.t =
  { scheme : string option
  ; authority : string option
  ; path : string
  ; query : string option
  ; fragment : string option
  }

let query t = Option.map Codec.decode t.query
let fragment t = Option.map Codec.decode t.fragment

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
  let path = Codec.decode path in
  match scheme with
  | None | Some ("http" | "https" | "file") ->
    String.add_prefix_if_not_exists path ~prefix:"/"
  | Some _ -> path
;;

let to_path ({ authority; scheme; _ } as t) =
  let path = decoded_path t in
  let authority = Option.value ~default:"" authority |> Codec.decode in
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
  if !win32 then slash_to_backslash path else path
;;

(* Both URI syntax and filesystem paths go through the same file normalization.
   Inputs here are decoded; the result is canonical encoded components. *)
let normalize_file ~authority ~path =
  let authority = String.lowercase_ascii authority in
  let path = String.add_prefix_if_not_exists path ~prefix:"/" in
  let path = if String.is_empty authority then lowercase_drive path else path in
  Codec.encode ~allow_slash:false authority, Codec.encode ~allow_slash:true path
;;

let of_string source =
  let { scheme; authority; path; query; fragment } = Uri_lexer.of_string source in
  let scheme = Option.map String.lowercase_ascii scheme in
  let path =
    match scheme, authority, path with
    | Some ("http" | "https"), Some _, "" -> "/"
    | _ -> path
  in
  let authority, path =
    match scheme with
    | Some "file" ->
      let authority, path =
        normalize_file
          ~authority:(Option.value ~default:"" authority |> Codec.decode)
          ~path:(Codec.decode path)
      in
      Some authority, path
    | None | Some _ ->
      Option.map (Codec.normalize Authority) authority, Codec.normalize Path path
  in
  { scheme
  ; authority
  ; path
  ; query = Option.map (Codec.normalize Query) query
  ; fragment = Option.map (Codec.normalize Fragment) fragment
  }
;;

let of_path path =
  let authority, path =
    if !win32 then Uri_lexer.split_unc_path (backslash_to_slash path) else "", path
  in
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
