(** Experimental client capabilities.

    The [experimental] field of [InitializeParams] is a free-form object that
    clients use to advertise non-standard capabilities. This module provides a
    small interface for querying boolean flags from it. *)

open Import

type t

(** [of_opt_json json] parses the [experimental] field of the initialize
    request. *)
val of_opt_json : Json.t option -> t

(** [bool t name] looks up the boolean flag [name]. Flags that are absent or
    are not booleans are treated as [false]. *)
val bool : t -> string -> bool
