(** Compatibility representation for LSP objects that can advertise
    deprecation through either the legacy field or a tag. *)
type 'tag t =
  { deprecated : bool option
  ; tags : 'tag list option
  }

(** Whether [tag] appears in the client's advertised value set according to
    [equal]. *)
val tag_supported : 'tag list -> tag:'tag -> equal:('tag -> 'tag -> bool) -> bool

(** Prefer the tag representation when supported, then fall back to the
    legacy [deprecated] field. *)
val create
  :  deprecated:bool
  -> tag:'tag
  -> supports_tag:bool
  -> supports_deprecated_field:bool
  -> 'tag t
