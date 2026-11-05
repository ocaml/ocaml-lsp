open Test.Import

val uri : Uri.t

(** [wire_uri] overrides the didOpen URI with raw text, bypassing normalization
    in the test client. *)
val test
  :  ?extra_env:string list
  -> ?capabilities:ClientCapabilities.t
  -> ?uri:Uri.t
  -> ?wire_uri:string
  -> ?language_id:string
  -> string
  -> (unit Client.t -> unit Fiber.t)
  -> unit
