open Import
open Types

let kind_to_string kind =
  match CodeActionKind.yojson_of_t kind with
  | `String kind -> kind
  | _ -> assert false
;;

let kind_is_requested only kind =
  match only with
  | None -> true
  | Some only ->
    let kind = kind_to_string kind in
    List.exists only ~f:(fun requested ->
      let requested = kind_to_string requested in
      String.equal requested kind || String.is_prefix kind ~prefix:(requested ^ "."))
;;
