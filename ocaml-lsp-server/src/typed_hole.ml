open Import

let in_range range holes =
  match range with
  | None -> holes
  | Some range -> List.filter ~f:(Lsp.Range.contains range) holes
;;

let find_prev ~range ~position holes =
  let holes = in_range range holes in
  List.fold_until
    ~init:None
    ~f:(fun prev hole ->
      if Lsp.Position.compare hole.end_ position < 0
      then Continue (Some hole)
      else Stop prev)
    ~finish:Fun.id
    holes
  |> function
  | None -> List.last holes
  | hole -> hole
;;

let find_next ~range ~position holes =
  let holes = in_range range holes in
  List.find ~f:(fun hole -> Lsp.Position.compare hole.start position > 0) holes
  |> function
  | None -> List.hd holes
  | hole -> hole
;;

let find ~range ~position ~direction holes =
  match direction with
  | `Prev -> find_prev ~range ~position holes
  | `Next -> find_next ~range ~position holes
;;

let all ?(pipeline_name = "typed-holes") merlin =
  Holes
  |> Document.Merlin.dispatch_exn ~name:pipeline_name merlin
  |> Fiber.map ~f:(List.map ~f:(fun (loc, _ty) -> Range.of_loc loc))
;;
