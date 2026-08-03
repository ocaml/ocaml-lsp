open Import

type t = (string * Json.t) list

let of_opt_json = function
  | Some (`Assoc fields) -> fields
  | _ -> []
;;

let bool t name =
  match List.assoc_opt name t with
  | Some (`Bool b) -> b
  | _ -> false
;;
