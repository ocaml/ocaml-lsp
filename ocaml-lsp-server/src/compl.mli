open Import

module Resolve : sig
  type t

  val uri : t -> Uri.t

  (** if the completion item doesn't have [data] field, then we don't resolve
      but return it *)
  val of_completion_item : CompletionItem.t -> t option

  include Json.Jsonable.S with type t := t
end

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
