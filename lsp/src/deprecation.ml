type 'tag t =
  { deprecated : bool option
  ; tags : 'tag list option
  }

let tag_supported value_set ~tag ~equal = List.exists (equal tag) value_set

let create ~deprecated ~tag ~supports_tag ~supports_deprecated_field =
  if not deprecated
  then { deprecated = None; tags = None }
  else if supports_tag
  then { deprecated = None; tags = Some [ tag ] }
  else if supports_deprecated_field
  then { deprecated = Some true; tags = None }
  else { deprecated = None; tags = None }
;;
