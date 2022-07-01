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
  let open Import in
  let scheme = scheme |> Option.value ~default:"file" in
  let authority =
    authority |> Option.map Uri.pct_decode |> Option.value ~default:""
  in
  let path =
    let path = path |> Uri.pct_decode in
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
| _* as path 
{ 
  let open Import in
  { scheme = "file"
  ; authority = ""
  ; path = String.add_prefix_if_not_exists path ~prefix:"/"
  }
}

{
  let of_string s =
    let lexbuf = Lexing.from_string s in
    uri lexbuf

  let of_path s =
    let lexbuf = Lexing.from_string s in
    path lexbuf
}
