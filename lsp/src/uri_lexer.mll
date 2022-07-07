{
type t =
  { scheme : string
  ; authority : string
  ; path : string
  }

let int_of_hex_char c =
  let c = int_of_char (Char.uppercase_ascii c) - 48 in
  if c > 9 then
    if c > 16 && c < 23 then Some (c - 7) else None
  else if c >= 0 then
    Some c
  else
    None

(* https://github.com/mirage/ocaml-uri/blob/master/lib/uri.ml#L318 *)
let decode b =
  let len = String.length b in
  let buf = Buffer.create len in
  let rec scan start cur =
    if cur >= len then
      Buffer.add_substring buf b start (cur - start)
    else if b.[cur] = '%' then (
      Buffer.add_substring buf b start (cur - start);
      let cur = cur + 1 in
      if cur >= len then
        Buffer.add_char buf '%'
      else
        match int_of_hex_char b.[cur] with
        | None ->
          Buffer.add_char buf '%';
          scan cur cur
        | Some highbits ->
          let cur = cur + 1 in
          if cur >= len then (
            Buffer.add_char buf '%';
            Buffer.add_char buf b.[cur - 1])
          else
            let start_at =
              match int_of_hex_char b.[cur] with
              | Some lowbits ->
                Buffer.add_char buf (Char.chr ((highbits lsl 4) + lowbits));
                cur + 1
              | None ->
                Buffer.add_char buf '%';
                Buffer.add_char buf b.[cur - 1];
                cur
            in
            scan start_at start_at)
    else
      scan start (cur + 1)
  in
  scan 0 0;
  Buffer.contents buf
}

rule uri = parse
([^':''/''?''#']+ as scheme ':') ?
("//" ([^'/''?''#']* as authority)) ?
([^'?''#']* as path)
{ 
  let open Import in
  let scheme = scheme |> Option.value ~default:"file" in
  let authority =
    authority |> Option.map decode |> Option.value ~default:""
  in
  let path =
    let path = path |> decode in
    match scheme with
    | "http" | "https" | "file" ->
      String.add_prefix_if_not_exists path ~prefix:"/"
    | _ -> path
  in
  { scheme; authority; path; } 
}

and path = parse
| "" { { scheme = "file"; authority = ""; path = "/" } }
| "//" ([^ '/']* as authority) (['/']_* as path) { { scheme = "file"; authority; path } }
| "//" ([^ '/']* as authority) { { scheme = "file"; authority; path = "/" } }
| ("/" _* as path) { { scheme = "file"; authority = ""; path } }
| (_* as path) { { scheme = "file"; authority = ""; path = "/" ^ path } }


{
  let of_string s =
    let lexbuf = Lexing.from_string s in
    uri lexbuf

  let of_path s =
    let lexbuf = Lexing.from_string s in
    path lexbuf
}
