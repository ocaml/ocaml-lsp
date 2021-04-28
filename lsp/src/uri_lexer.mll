{
type t =
  { scheme : string option
  ; authority : string
  ; path : string
  }
}

rule path = parse
| '/'? { path1 (Buffer.create 12) lexbuf }
and path1 buf = parse
| '\\' { Buffer.add_char buf '/' ; path1 buf lexbuf }
| "%5" ['c' 'C'] { Buffer.add_char buf '/' ; path1 buf lexbuf }
| "%3" ['a' 'A'] { Buffer.add_char buf ':' ; path1 buf lexbuf }
| "%3" ['d' 'D'] { Buffer.add_char buf '=' ; path1 buf lexbuf }
| "%3" ['f' 'F'] { Buffer.add_char buf '?' ; path1 buf lexbuf }
| "%20" { Buffer.add_char buf ' ' ; path1 buf lexbuf }
| _ as c { Buffer.add_char buf c ; path1 buf lexbuf }
| eof { Buffer.contents buf }

and uri = parse
| ([^ ':']+) as scheme ':' { uri1 (Some scheme) lexbuf }
| "" { uri1 None lexbuf }
and uri1 scheme = parse
| "//" ([^ '/']* as authority) { uri2 scheme authority lexbuf }
| "" { uri2 scheme "" lexbuf }
and uri2 scheme authority = parse
| "" { { scheme ; authority ; path = path lexbuf } }

{
  let escape_path s =
    let lexbuf = Lexing.from_string s in
    path lexbuf

  let of_string s =
    let lexbuf = Lexing.from_string s in
    uri lexbuf
}
