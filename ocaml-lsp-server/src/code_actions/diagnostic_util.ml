open Import

(* Merlin renders a warning as "Warning 26: ..." and, when warnings are fatal,
   as "Error (warning 26): ...". Alerts follow the same pair of shapes. *)
let is_warning s ~number =
  String.is_prefix s ~prefix:(sprintf "Warning %d" number)
  || String.is_prefix s ~prefix:(sprintf "Error (warning %d)" number)
;;

let is_unused_var_warning s = is_warning s ~number:26 || is_warning s ~number:27

let is_deprecated_warning s =
  String.is_prefix s ~prefix:"Alert deprecated"
  || String.is_prefix s ~prefix:"Error (alert deprecated)"
;;
