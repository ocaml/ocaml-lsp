type t

type invalid_utf =
  | Malformed of string
  | Insufficient_input

exception Invalid_utf of invalid_utf

val of_string : string -> t
val squash : t -> t * string
val to_string : t -> string
val to_string_debug : t -> string

(* [insert t s] right of the current position *)
val insert : t -> string -> t
val goto_line : t -> int -> t
val goto_position : t -> Position.t -> [ `UTF16 | `UTF8 ] -> t
val add_buffer_between : Buffer.t -> t -> t -> unit
val goto_end : t -> t
val drop_until : t -> t -> t
val apply_change : t -> Types.Range.t -> [ `UTF16 | `UTF8 ] -> replacement:string -> t
val offset : t -> int

module Private : sig
  type zipper := t

  type nonrec t =
    { left : Substring.t list
    ; rel_pos : int
    ; abs_pos : int
    ; current : Substring.t
    ; line : int
    ; right : Substring.t list
    }

  val reflect : zipper -> t
end
