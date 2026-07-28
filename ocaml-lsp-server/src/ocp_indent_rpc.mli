open Import

type t

type error =
  [ `Binary_not_found
  | `Msg of string
  ]

val create : unit -> t
val stop : t -> unit Fiber.t

val indentation
  :  t
  -> path:string
  -> line:int
  -> source:string
  -> syntaxes:string list
  -> (int, error) result Fiber.t
