open Import

let holes_lst doc =
  let open Fiber.O in
  let+ holes_lst = Document.dispatch_exn doc Holes in
  List.map holes_lst ~f:(fun (loc, str) -> (Range.of_loc loc, `Type str))

let next_hole doc (pos : Position.t) =
  let open Fiber.O in
  let+ holes = holes_lst doc in
  let is_in_range { Position.line; character } { Range.start; end_ } =
    (line == start.line && character >= start.character)
    || (line == end_.line && character <= end_.character)
    || (line > start.line && line < end_.line)
  in
  let holes_sorted =
    List.sort holes ~compare:(fun (range1, _) (range2, _) ->
        Range.compare range1 range2)
  in
  (* we want to find such a range that starts after the current position *)
  List.find holes_sorted ~f:(fun (range, _) ->
      match Position.compare pos range.Range.start with
      | Ordering.Lt -> true
      | Gt -> false
      | Eq ->
        (* we don't want the same range that we're in now; we need the next one *)
        not (is_in_range pos range))
  |> (function
       (* if the current position is larger than all other ranges, we cycle back
          to first hole in the file *)
       | None -> List.hd_opt holes_sorted
       | Some _ as o -> o)
  |> Option.map ~f:fst
