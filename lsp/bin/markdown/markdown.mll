{
  open Import
}

let ws = [' ' '\t' '\r']*

rule md acc b = parse
  | "```typescript" ws "\n" { ts acc b lexbuf }
  | eof { acc }
  | _ { md acc b lexbuf }

and ts acc b = parse
  (* We parse typescript comments because they can include mrakdown fragments *)
  | "/*" as s { Buffer.add_string b s; comment acc b lexbuf }
  | "```"
    { let snippet = Buffer.contents b in
      Buffer.clear b;
      md (snippet :: acc) b lexbuf
    }
  | "\r\n" { Buffer.add_string b "\n"; ts acc b lexbuf }
  | _ as c
    { Buffer.add_char b c;
      ts acc b lexbuf
    }
  | eof { Code_error.raise "unterminated typescript snippet" [] }

and comment acc b = parse
  | "*/" as s { Buffer.add_string b s; ts acc b lexbuf }
  | "\r\n" { Buffer.add_string b "\n"; comment acc b lexbuf }
  | _ as c { Buffer.add_char b c; comment acc b lexbuf }
  | eof { failwith "unterminated comment" }

{
  let read_typescript lexbuf =
    let b = Buffer.create 512 in
    List.rev (md [] b lexbuf)
}
