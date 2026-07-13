open Test.Import

val uri : Uri.t

val test
  :  ?extra_env:string list
  -> ?capabilities:ClientCapabilities.t
  -> ?uri:Uri.t
  -> ?language_id:string
  -> string
  -> (unit Client.t -> unit Fiber.t)
  -> unit
