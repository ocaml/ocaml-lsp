{
type t =
  { scheme : string
  ; authority : string
  ; path : string
  }
}

rule uri = parse
([^':''/''?''#']+ as scheme ':') ?
("//" ([^'/''?''#']* as authority)) ?
([^'?''#']* as path)
{ 
  let scheme = scheme |> Option.value ~default:"file" in
  let authority =
    authority |> Option.map Uri.pct_decode |> Option.value ~default:""
  in
  let path =
    let path = path |> Uri.pct_decode in
    match scheme with
    | "http" | "https" | "file" ->
      if Import.String.is_prefix path ~prefix:"/" then path else "/" ^ path
    | _ -> path
  in
  {scheme ; authority; path;} 
}

{
  let of_string s =
    let lexbuf = Lexing.from_string s in
    uri lexbuf
}
