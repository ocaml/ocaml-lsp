open Import

type state =
  | Binary_not_found
  | Out_of_date
  | Running

type t

val state : t -> state Fiber.t

val create : Scheduler.t -> t
