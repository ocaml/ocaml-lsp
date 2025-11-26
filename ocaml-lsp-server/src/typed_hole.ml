open Import

let syntax_repr = "_"
let can_be_hole s = String.equal syntax_repr s

(* the pattern matching below is taken and modified (minimally, to adapt the
   return type) from [Query_commands.dispatch]'s [Construct] branch;

   If we directly dispatched [Construct] command to merlin, we'd be doing
   useless computations: we need info whether the expression at the cursor is a
   hole, we don't need constructed expressions yet.

   Ideally, merlin should return a callback [option], which is [Some] when the
   context is applicable. *)
let is_a_hole = function
  | (_, Browse_raw.Module_expr { mod_desc = Tmod_hole; _ }) :: (_, _) :: _
  | (_, Browse_raw.Expression { exp_desc = Texp_hole; _ }) :: _ -> true
  | [] | (_, _) :: _ -> false
;;

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

let all ?(pipeline_name = "typed-holes") merlin =
  Holes
  |> Document.Merlin.dispatch_exn ~name:pipeline_name merlin
  |> Fiber.map ~f:(List.map ~f:(fun (loc, _ty) -> Range.of_loc loc))
;;
