open Import

let in_range range holes =
  match range with
  | None -> holes
  | Some range -> List.filter ~f:(Range.contains range) holes
;;

let find_prev ~range ~position holes =
  let holes = in_range range holes in
  Base.List.fold_until
    ~init:None
    ~f:(fun prev hole ->
      match Position.compare hole.end_ position with
      | Lt -> Continue (Some hole)
      | Gt | Eq -> Stop prev)
    ~finish:Fun.id
    holes
  |> function
  | None -> Base.List.last holes
  | hole -> hole
;;

let find_next ~range ~position holes =
  let holes = in_range range holes in
  List.find
    ~f:(fun hole ->
      match Position.compare hole.start position with
      | Gt -> true
      | Lt | Eq -> false)
    holes
  |> function
  | None -> Base.List.hd holes
  | hole -> hole
;;

let find ~range ~position ~direction holes =
  match direction with
  | `Prev -> find_prev ~range ~position holes
  | `Next -> find_next ~range ~position holes
;;

let all ?(name = "typed-holes") merlin =
  Holes
  |> Document.Merlin.dispatch_exn ~name merlin
  |> Fiber.map ~f:(List.map ~f:(fun (loc, _ty) -> Range.of_loc loc))
;;
