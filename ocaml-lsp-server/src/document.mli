open Lsp.Types

type t

module Syntax : sig
  type t =
    | Ocaml
    | Reason
end

module Kind : sig
  type t =
    | Intf
    | Impl
end

val kind : t -> Kind.t

val syntax : t -> Syntax.t

val make : DidOpenTextDocumentParams.t -> t

val uri : t -> Lsp.Uri.t

val source : t -> Msource.t

val with_pipeline : t -> (Mpipeline.t -> 'a) -> 'a

val version : t -> int

val update_text : ?version:int -> TextDocumentContentChangeEvent.t -> t -> t

val dispatch : t -> 'a Query_protocol.t -> 'a
