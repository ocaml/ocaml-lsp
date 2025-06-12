open Import

let is_unused_var_warning s =
  String.is_prefix s ~prefix:"Error (warning 26)"
  || String.is_prefix s ~prefix:"Error (warning 27)"
;;

let is_deprecated_warning s = String.is_prefix s ~prefix:"Error (alert deprecated)"
