open Import

type t

module Syntax : sig
  type t =
    | Ocaml
    | Reason
    | Ocamllex
    | Menhir

  val human_name : t -> string

  val markdown_name : t -> string

  val of_fname : string -> t
end

module Kind : sig
  type t =
    | Intf
    | Impl

  val of_fname : string -> t
end

val kind : t -> Kind.t

val syntax : t -> Syntax.t

val make :
  Scheduler.timer -> Scheduler.thread -> DidOpenTextDocumentParams.t -> t

val timer : t -> Scheduler.timer

val uri : t -> Uri.t

val source : t -> Msource.t

val with_pipeline : t -> (Mpipeline.t -> 'a) -> ('a, exn) result Fiber.t

val with_pipeline_exn : t -> (Mpipeline.t -> 'a) -> 'a Fiber.t

val version : t -> int

val update_text : ?version:int -> TextDocumentContentChangeEvent.t -> t -> t

val dispatch : t -> 'a Query_protocol.t -> ('a, exn) result Fiber.t

val dispatch_exn : t -> 'a Query_protocol.t -> 'a Fiber.t
