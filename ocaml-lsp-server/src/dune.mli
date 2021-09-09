open! Import

type run =
  | Binary_not_found
  | Out_of_date

type t

val view_promotion_capability : string * Json.t

val run : t -> (unit, run) result Fiber.t

val create :
     build_dir:string
  -> ClientCapabilities.t
  -> Diagnostics.t
  -> Progress.t
  -> t Fiber.t

val stop : t -> unit Fiber.t
