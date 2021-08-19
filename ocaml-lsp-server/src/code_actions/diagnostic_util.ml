open Import

let is_unused_var_warning (d : Diagnostic.t) =
  String.is_prefix d.message ~prefix:"Error (warning 26)"
  || String.is_prefix d.message ~prefix:"Error (warning 27)"
