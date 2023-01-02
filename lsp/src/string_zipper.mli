type t

val of_string : string -> t

val to_string : t -> string

val to_string_debug : t -> string

(* [insert t s] right of the current position *)
val insert : t -> string -> t

val goto_line : t -> int -> t

val drop_until : t -> t -> t

val apply_change :
  t -> Types.Range.t -> [ `UTF16 | `UTF8 ] -> replacement:string -> t

module Private : sig
  type zipper := t

  type nonrec t =
    { left : Substring.t list
    ; rel_pos : int
    ; current : Substring.t
    ; line : int
    ; right : Substring.t list
    }

  val reflect : zipper -> t
end
