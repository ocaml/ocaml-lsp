open Import

type t

val create : unit -> t
val stop : t -> unit Fiber.t

val format_type
  :  t
  -> typ:string
  -> (string, [> `Msg of string | `No_process ]) result Fiber.t

val format_doc
  :  t
  -> Document.t
  -> (TextEdit.t list, [> `Msg of string | `No_process | `No_V2 ]) result Fiber.t

val run
  :  logger:(type_:MessageType.t -> message:string -> unit Fiber.t)
  -> t
  -> (unit, [> `Disabled | `Binary_not_found ]) result Fiber.t
