{
type t =
  { scheme : string option
  ; authority : string
  ; path : string
  }
}

rule uri = parse
| ([^ ':']+) as scheme ':' { uri1 (Some scheme) lexbuf }
| "" { uri1 None lexbuf }
and uri1 scheme = parse
| "//" ([^ '/']* as authority) { uri2 scheme authority lexbuf }
| "" { uri2 scheme "" lexbuf }
and uri2 scheme authority = parse
| (_ *) as path eof { { scheme ; authority ; path } }

{
  let of_string s =
    let lexbuf = Lexing.from_string s in
    uri lexbuf
}
