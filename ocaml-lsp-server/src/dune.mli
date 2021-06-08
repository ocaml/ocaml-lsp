open! Import

type run =
  | Binary_not_found
  | Out_of_date

type t

val run : t -> (unit, run) result Fiber.t

val create : Diagnostics.t -> Progress.t -> t

val stop : t -> unit Fiber.t
