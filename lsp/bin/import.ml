let sprintf = Printf.sprintf

module Option = struct
  include Option

  let map t ~f = Option.map f t

  let value_exn = function
    | None -> assert false
    | Some s -> s
  ;;
end

module List = struct
  include ListLabels

  type ('a, 'b) skip_or_either =
    | Skip
    | Left of 'a
    | Right of 'b

  let rev_filter_partition_map =
    let rec loop l accl accr ~f =
      match l with
      | [] -> accl, accr
      | x :: l ->
        (match f x with
         | Skip -> loop l accl accr ~f
         | Left y -> loop l (y :: accl) accr ~f
         | Right y -> loop l accl (y :: accr) ~f)
    in
    fun l ~f -> loop l [] [] ~f
  ;;

  let filter_partition_map l ~f =
    let l, r = rev_filter_partition_map l ~f in
    rev l, rev r
  ;;
end

module String = struct
  include StringLabels

  let to_dyn = Dyn.string

  module Map = struct
    include MoreLabels.Map.Make (String)

    let of_list_reducei xs ~f =
      List.fold_left xs ~init:empty ~f:(fun map (k, v) ->
        update map ~key:k ~f:(function
          | None -> Some v
          | Some v' -> Some (f k v v')))
    ;;

    let of_list_map_exn xs ~f = List.map xs ~f |> of_list
    let union_exn x y = union ~f:(fun _ _ _ -> assert false) x y
  end
end

module Code_error = struct
  let raise name data =
    invalid_arg (sprintf "%s %s" name (Dyn.to_string (Dyn.record data)))
  ;;
end

module Poly = struct
  let equal = Stdlib.( = )
  let compare = Stdlib.compare
end
