module Array = ArrayLabels
module List = ListLabels
module String = StringLabels

type t =
  | Atom of string
  | List of t list

let rec to_string = function
  | Atom s -> Escape.quote_if_needed s
  | List l ->
    Printf.sprintf "(%s)" (List.map ~f:to_string l |> String.concat ~sep:" ")

let rec pp ppf = function
  | Atom s -> Format.pp_print_string ppf (Escape.quote_if_needed s)
  | List [] -> Format.pp_print_string ppf "()"
  | List (first :: rest) ->
    Format.pp_open_box ppf 1;
    Format.pp_print_string ppf "(";
    Format.pp_open_hvbox ppf 0;
    pp ppf first;
    List.iter rest ~f:(fun sexp ->
        Format.pp_print_space ppf ();
        pp ppf sexp);
    Format.pp_close_box ppf ();
    Format.pp_print_string ppf ")";
    Format.pp_close_box ppf ()

let hash = Stdlib.Hashtbl.hash

let string_equal (x : string) (y : string) = x = y

let rec equal x y =
  match (x, y) with
  | Atom x, Atom y -> string_equal x y
  | List x, List y -> equal_list x y
  | _, _ -> false

and equal_list xs ys =
  (* replicating List.equal to avoid circular deps *)
  match (xs, ys) with
  | [], [] -> true
  | x :: xs, y :: ys -> equal x y && equal_list xs ys
  | _, _ -> false

let compare x y = Ordering.of_int (compare x y)

let rec of_dyn : Dyn.t -> t = function
  | Opaque -> Atom "<opaque>"
  | Unit -> List []
  | Int i -> Atom (string_of_int i)
  | Bool b -> Atom (string_of_bool b)
  | String s -> Atom s
  | Bytes s -> Atom (Bytes.to_string s)
  | Char c -> Atom (String.make 1 c)
  | Float f -> Atom (string_of_float f)
  | Option o ->
    List
      ( match o with
      | None -> []
      | Some x -> [ of_dyn x ] )
  | List l -> List (List.map l ~f:of_dyn)
  | Array a -> List (Array.to_list a |> List.map ~f:of_dyn)
  | Map xs -> List (List.map xs ~f:(fun (k, v) -> List [ of_dyn k; of_dyn v ]))
  | Set xs -> List (List.map xs ~f:of_dyn)
  | Tuple t -> List (List.map t ~f:of_dyn)
  | Record fields ->
    List (List.map fields ~f:(fun (field, f) -> List [ Atom field; of_dyn f ]))
  | Variant (s, []) -> Atom s
  | Variant (s, xs) -> List (Atom s :: List.map xs ~f:of_dyn)
