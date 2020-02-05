{
  let b = Buffer.create 256
}

rule md acc = parse
  | eof { acc }
  | _ { acc }

{
  let read_typescript lexbuf =
    Buffer.clear b;
    List.rev (md [] lexbuf)
}
