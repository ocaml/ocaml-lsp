{

(* Components retain their encoded spelling. In particular, None and Some ""
   distinguish an absent delimiter from a present but empty component. *)
type t =
  { scheme : string option
  ; authority : string option
  ; path : string
  ; query : string option
  ; fragment : string option
  }
}

rule uri = parse
([^':' '/' '?' '#']+ as scheme ':') ?
("//" ([^ '/' '?' '#']* as authority)) ?
([^ '?' '#']* as path)
('?' ([^ '#']* as query)) ?
('#' (_ * as fragment)) ?
{ { scheme; authority; path; query; fragment } }

(* Filesystem paths are not URI syntax. Split only a leading UNC authority;
   all data, including percent signs and relative paths, is left untouched. *)
and unc_path = parse
| "//" ([^ '/']* as authority) ('/' _* as path) { authority, path }
| "//" ([^ '/']* as authority) { authority, "" }
| (_* as path) { "", path }

{
  let of_string s = uri (Lexing.from_string s)
  let split_unc_path s = unc_path (Lexing.from_string s)
}
