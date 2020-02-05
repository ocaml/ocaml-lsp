{
  open Import
}

let ws = [' ' '\t' '\r']*

rule md acc b = parse
  | "```typescript" ws "\n" { ts acc b lexbuf }
  | eof { acc }
  | _ { md acc b lexbuf }

and ts acc b = parse
  | "```"
    { let snippet = Buffer.contents b in
      Buffer.clear b;
      md (snippet :: acc) b lexbuf
    }
  | _ as c
    { Buffer.add_char b c;
      ts acc b lexbuf
    }
  | eof { Code_error.raise "unterminated typescript snippet" [] }

{
  let read_typescript lexbuf =
    let b = Buffer.create 512 in
    List.rev (md [] b lexbuf)
}
