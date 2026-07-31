type 'tag t =
  { deprecated : bool option
  ; tags : 'tag list option
  }

val tag_supported : 'tag list -> tag:'tag -> bool

val create
  :  deprecated:bool
  -> tag:'tag
  -> supports_tag:bool
  -> supports_deprecated_field:bool
  -> 'tag t
