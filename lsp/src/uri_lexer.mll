{

open Import

(* Components retain their encoded spelling. In particular, None and Some ""
   distinguish an absent delimiter from a present but empty component. *)
type t =
  { scheme : string option
  ; authority : string option
  ; path : string
  ; query : string option
  ; fragment : string option
  }

let int_of_hex_char = function
  | '0' .. '9' as c -> Some (Char.code c - Char.code '0')
  | 'a' .. 'f' as c -> Some (Char.code c - Char.code 'a' + 10)
  | 'A' .. 'F' as c -> Some (Char.code c - Char.code 'A' + 10)
  | _ -> None

let is_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

(* Full decoding is for component access and filesystem conversion. Normalization
   only decodes unreserved characters and uppercases the remaining escape digits.
   In that mode, quote malformed '%' bytes: otherwise decoding their neighbours
   can introduce a new escape, e.g. %%36%31 would become %61 and then decode as 'a'. *)
let decode ?(only_unreserved = false) s =
  if not (String.contains s '%') then s
  else
    let len = String.length s in
    let buf = Buffer.create len in
    let rec scan i =
      if i < len then
        let escape =
          if s.[i] = '%' && i + 2 < len then
            match int_of_hex_char s.[i + 1], int_of_hex_char s.[i + 2] with
            | Some high, Some low -> Some (Char.chr ((high lsl 4) + low))
            | _ -> None
          else None
        in
        match escape with
        | Some c ->
          if only_unreserved && not (is_unreserved c) then (
            Buffer.add_char buf '%';
            Buffer.add_char buf (Char.uppercase_ascii s.[i + 1]);
            Buffer.add_char buf (Char.uppercase_ascii s.[i + 2]))
          else Buffer.add_char buf c;
          scan (i + 3)
        | None ->
          if only_unreserved && s.[i] = '%' then Buffer.add_string buf "%25"
          else Buffer.add_char buf s.[i];
          scan (i + 1)
    in
    scan 0;
    Buffer.contents buf
}

rule uri = parse
([^':' '/' '?' '#']+ as scheme ':') ?
("//" ([^ '/' '?' '#']* as authority)) ?
([^ '?' '#']* as path)
('?' ([^ '#']* as query)) ?
('#' (_ * as fragment)) ?
{ { scheme; authority; path; query; fragment } }

(* Filesystem paths are not URI syntax: '%' is a literal character and only
   the leading UNC authority is split off here. Encoding belongs to of_path. *)
and path = parse
| "" { "", "/" }
| "//" ([^ '/']* as authority) (['/']_* as path) { authority, path }
| "//" ([^ '/']* as authority) { authority, "/" }
| ("/" _* as path) { "", path }
| (_* as path) { "", "/" ^ path }

{
  let of_string s = uri (Lexing.from_string s)
  let of_path s = path (Lexing.from_string s)
}
