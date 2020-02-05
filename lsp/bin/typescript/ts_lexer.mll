{
  open! Import
  open Ts_token
}

let ws = [' ' '\t' '\n']+

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
  | ['0' - '9']* '.' ['0' - '9']+ { Float (Option.value_exn (Float.of_string (Lexing.lexeme lexbuf))) }
  | ['0' - '9']+ as i { Int (Int.of_string_exn i) }
  | '"' ([^ '"']* as s) '"' { String s }
  | '\'' ([^ '\'']* as s) '\'' { String s }
  | ws { token lexbuf }

and comment = parse
  | "*/" { token lexbuf }
  | _ { comment lexbuf }
{
}
