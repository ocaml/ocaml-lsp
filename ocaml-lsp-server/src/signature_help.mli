open Merlin_kernel
open Ocaml_parsing
open Ocaml_typing

type parameter_info =
  { label : Asttypes.arg_label
  ; param_start : int
  ; param_end : int
  ; argument : Typedtree.expression option
  }

type application_signature =
  { function_name : string option
  ; function_position : Msource.position
  ; signature : string
  ; parameters : parameter_info list
  ; active_param : int option
  }

val application_signature :
     prefix:string
  -> ('a * Merlin_specific.Browse_raw.node) list
  -> application_signature option
