{
  open! Import
  open Ts_token
}

let ws = [' ' '\t' '\n']+

let digit = ['0' - '9']
let ident_first = ['a' - 'z' 'A' - 'Z' '_' ]
let ident_char = ident_first | digit

rule token = parse
  | "//" [^ '\n']* '\n' { token lexbuf }
  | "/*" { comment lexbuf }
  | "null" { Null }
  | "extends" { Extends }
  | "const" { Const }
  | "export" { Export }
  | "type" { Type }
  | "interface" { Interface }
  | "namespace" { Namespace }
  | '|' { Alt }
  | ';' { Semicolon }
  | '(' { Paren L }
  | ')' { Paren R }
  | '{' { Curly L }
  | '}' { Curly R }
  | ':' { Colon }
  | '=' { Equal }
  | '<' { Angle L }
  | ',' { Comma }
  | '>' { Angle R }
  | "[]" { Array_type }
  | '[' { Square L }
  | ']' { Square R }
  | '?' { Question }
  | '-'? digit* '.' digit+ { Float (Option.value_exn (Float.of_string (Lexing.lexeme lexbuf))) }
  | '-'? digit+ as i { Int (Int.of_string_exn i) }
  | '"' ([^ '"']* as s) '"' { String s }
  | '\'' ([^ '\'']* as s) '\'' { String s }
  | ident_first ident_char* { Ident (Lexing.lexeme lexbuf) }
  | ws { token lexbuf }

and comment = parse
  | "*/" { token lexbuf }
  | _ { comment lexbuf }
{
}
