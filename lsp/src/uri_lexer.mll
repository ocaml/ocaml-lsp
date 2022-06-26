{
type t =
  { scheme : string option
  ; authority : string option
  ; path : string
  }
}

rule uri = parse
([^':''/''?''#']+ as scheme ':') ?
("//" ([^'/''?''#']* as authority)) ?
([^'?''#']* as path)
{ {scheme ; authority; path;} }

{
  let of_string s =
    let lexbuf = Lexing.from_string s in
    uri lexbuf
}
