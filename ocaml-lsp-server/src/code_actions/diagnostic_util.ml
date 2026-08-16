open Import

(* Merlin renders a warning as "Warning 26: ..." and, when warnings are fatal,
   as "Error (warning 26 [unused-var]): ..."; older renderings omit the warning
   name. Alerts follow the same pair of shapes. *)
let is_warning s ~number =
  [ sprintf "Warning %d:" number
  ; sprintf "Warning %d [" number
  ; sprintf "Error (warning %d)" number
  ; sprintf "Error (warning %d " number
  ]
  |> List.exists ~f:(fun prefix -> String.is_prefix s ~prefix)
;;

let is_unused_var_warning s = is_warning s ~number:26 || is_warning s ~number:27

let is_deprecated_warning s =
  String.is_prefix s ~prefix:"Alert deprecated"
  || String.is_prefix s ~prefix:"Error (alert deprecated)"
;;
