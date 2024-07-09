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

val syntax : t -> Syntax.t

module Single_pipeline : sig
  type t

  val create : Lev_fiber.Thread.t -> t
end

val make
  :  Lev_fiber.Timer.Wheel.t
  -> Merlin_config.DB.t
  -> Single_pipeline.t
  -> DidOpenTextDocumentParams.t
  -> position_encoding:[ `UTF8 | `UTF16 ]
  -> t Fiber.t

val uri : t -> Uri.t
val text : t -> string
val source : t -> Msource.t

module Merlin : sig
  type doc := t
  type t

  val source : t -> Msource.t
  val timer : t -> Lev_fiber.Timer.Wheel.task

  (** uses a single pipeline, provisioned by the configuration attached to the
      merlin document (via {!type:t}). *)
  val with_pipeline_exn : ?name:string -> t -> (Mpipeline.t -> 'a) -> 'a Fiber.t

  (** Like {!val:with_pipeline_exn} but where the merlin configuration is
      supplied manually. If, for example, it is computed outside the execution
      of the pipeline.*)
  val with_configurable_pipeline_exn
    :  ?name:string
    -> config:Mconfig.t
    -> t
    -> (Mpipeline.t -> 'a)
    -> 'a Fiber.t

  val dispatch
    :  ?name:string
    -> t
    -> 'a Query_protocol.t
    -> ('a, Exn_with_backtrace.t) result Fiber.t

  val dispatch_exn : ?name:string -> t -> 'a Query_protocol.t -> 'a Fiber.t

  val doc_comment
    :  ?name:string
    -> t
    -> Msource.position
    -> (* doc string *)
    string option Fiber.t

  val syntax_doc
    :  Mpipeline.t
    -> Msource.position
    -> Query_protocol.syntax_doc_result option

  type type_enclosing =
    { loc : Loc.t
    ; typ : string
    ; doc : string option
    ; syntax_doc : Query_protocol.syntax_doc_result option
    }

  val type_enclosing
    :  ?name:string
    -> t
    -> Msource.position
    -> (* verbosity *) int
    -> with_syntax_doc:bool
    -> type_enclosing option Fiber.t

  val kind : t -> Kind.t
  val to_doc : t -> doc
  val mconfig : t -> Mconfig.t Fiber.t
end

val kind : t -> [ `Merlin of Merlin.t | `Other ]
val merlin_exn : t -> Merlin.t
val version : t -> int
val update_text : ?version:int -> t -> TextDocumentContentChangeEvent.t list -> t
val close : t -> unit Fiber.t

(** [get_impl_intf_counterparts uri] returns the implementation/interface
    counterparts for the URI [uri].

    For instance, the counterparts of the file [/file.ml] are [/file.mli]. *)
val get_impl_intf_counterparts : Merlin.t option -> Uri.t -> Uri.t list

(** [edits t edits] creates a [WorkspaceEdit.t] that applies edits [edits] to
    the document [t]. *)
val edit : t -> TextEdit.t list -> WorkspaceEdit.t

(** [substring t range] returns the substring of the document [t] that
    corresponds to the range [range].

    Returns [None] when there is no corresponding substring. *)
val substring : t -> Range.t -> string option
