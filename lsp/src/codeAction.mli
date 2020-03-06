open Import
open Protocol

module Kind : sig
  type t =
    | Empty
    | QuickFix
    | Refactor
    | RefactorExtract
    | RefactorInline
    | RefactorRewrite
    | Source
    | SourceOrganizeImports
    | Other of string

  include Json.Jsonable.S with type t := t
end

module Context : sig
  type t =
    { diagnostics : PublishDiagnostics.diagnostic list
    ; only : Kind.t Only.t
    }
end

module Params : sig
  type t =
    { textDocument : TextDocumentIdentifier.t
    ; range : Range.t
    ; context : Context.t
    }

  include Json.Jsonable.S with type t := t
end

type t =
  { title : string
  ; kind : Kind.t option
  ; diagnostics : PublishDiagnostics.diagnostic list
  ; edit : WorkspaceEdit.t option
  ; command : Command.t option
  ; isPreferred : bool
  }

val yojson_of_t : t -> Json.t

type result = (Command.t, t) Either.t list

val yojson_of_result : result -> Json.t
