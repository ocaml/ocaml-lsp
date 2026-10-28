open Import

module Resolve : sig
  type t

  val uri : t -> Uri.t

  (** if the completion item doesn't have [data] field, then we don't resolve
      but return it *)
  val of_completion_item : CompletionItem.t -> t option

  include Json.Jsonable.S with type t := t
end

module Complete_by_prefix : sig
  module Function_label : sig
    type t =
      { name : string
      ; type_ : string
      }
  end

  type t =
    { entries : CompletionItem.t list
    ; application_labels : Function_label.t list option
      (** [None] unless the position is in a function application. *)
    }

  (** Use Merlin to obtain completions for [prefix] at the given position.
      Unlike [textDocument/completion], in the case that Merlin reports that
      the position is a function application context, do not squash function
      argument names into the completions list. Instead, return those function
      argument names and their types in [application_labels].

      Results don't depend on the client's completion capabilities: no snippet
      items, no deprecation fields or tags, no [data] for
      [completionItem/resolve], and variants always have kind [EnumMember]. *)
  val complete_nosquash : Document.Merlin.t -> prefix:string -> Position.t -> t Fiber.t
end

(** Creates a server response for ["textDocument/completion"]. *)
val complete
  :  State.t
  -> CompletionParams.t
  -> [> `CompletionList of CompletionList.t ] option Fiber.t

(** creates a server response for ["completionItem/resolve"] *)
val resolve
  :  Document.Merlin.t
  -> CompletionItem.t
  -> Resolve.t
  -> (Document.Merlin.t -> [> `Logical of int * int ] -> string option Fiber.t)
  -> markdown:bool
  -> CompletionItem.t Fiber.t

(** [prefix_of_position ~short_path source position] computes prefix before
    given [position]. A prefix is essentially a piece of code that refers to one
    thing eg a single infix operator "|>", a single reference to a function or
    variable: "List.map" a keyword "let" etc If there is semantically irrelivent
    whitespace it is removed eg "List. map"->"List.map"

    @param short_path
      determines whether we want full prefix or cut at ["."], e.g.
      [List.m<cursor>] returns ["m"] when [short_path] is set vs ["List.m"] when
      not.
    @return prefix of [position] in [source] and its length *)
val prefix_of_position : short_path:bool -> Msource.t -> [< Msource.position ] -> string

(** [reconstruct_ident source position] returns the identifier at [position].
    Note: [position] can be in the middle of the identifier.

    @return identifier unless none is found *)
val reconstruct_ident : Msource.t -> [< Msource.position ] -> string option

module For_tests : sig
  val sortText_of_index : item_count:int -> int -> string
end
