open Import

type t

module Syntax : sig
  type t =
    | Ocaml
    | Reason
    | Ocamllex
    | Menhir
    | Cram
    | Dune

  val human_name : t -> string

  val markdown_name : t -> string
end

module Kind : sig
  type t =
    | Intf
    | Impl
end

val is_merlin : t -> bool

val kind : t -> Kind.t

val syntax : t -> Syntax.t

val make :
     Lev_fiber.Timer.Wheel.t
  -> Merlin_config.t
  -> merlin_thread:Lev_fiber.Thread.t
  -> DidOpenTextDocumentParams.t
  -> t Fiber.t

val timer : t -> Lev_fiber.Timer.Wheel.task

val uri : t -> Uri.t

val text : t -> string

val source : t -> Msource.t

val with_pipeline_exn : t -> (Mpipeline.t -> 'a) -> 'a Fiber.t

val version : t -> int

val update_text :
  ?version:int -> t -> TextDocumentContentChangeEvent.t list -> t

val dispatch :
  t -> 'a Query_protocol.t -> ('a, Exn_with_backtrace.t) result Fiber.t

val dispatch_exn : t -> 'a Query_protocol.t -> 'a Fiber.t

val close : t -> unit Fiber.t

(** [get_impl_intf_counterparts uri] returns the implementation/interface
    counterparts for the URI [uri].

    For instance, the counterparts of the file [/file.ml] are [/file.mli]. *)
val get_impl_intf_counterparts : Uri.t -> Uri.t list

val edit : t -> TextEdit.t -> WorkspaceEdit.t
