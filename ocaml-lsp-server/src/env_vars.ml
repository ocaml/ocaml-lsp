open Stdune.Option.O

let _TEST () : bool option =
  let+ v = Sys.getenv_opt "OCAMLLSP_TEST" in
  match v with
  | "true" -> true
  | "false" -> false
  | unexpected_val ->
    Format.eprintf
      "invalid value %S for OCAMLLSP_TEST ignored. Only true or false are allowed@."
      unexpected_val;
    false
;;

let _IS_HOVER_EXTENDED () : bool option =
  let* v = Sys.getenv_opt "OCAMLLSP_HOVER_IS_EXTENDED" in
  match v with
  | "true" | "1" -> Some true
  | _ -> Some false
;;
