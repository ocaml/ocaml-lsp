open Import

let position_in_comment ~position ~merlin =
  let loc_contains_position (_, (loc : Loc.t)) =
    let start = Position.of_lexical_position loc.loc_start in
    let end_ = Position.of_lexical_position loc.loc_end in
    match Option.both start end_ with
    | Some (start, end_) ->
      let range = Range.create ~start ~end_ in
      (match Position.compare_inclusion position range with
       | `Inside -> true
       | `Outside _ -> false)
    | None -> false
  in
  Document.Merlin.with_pipeline_exn ~name:"get-comments" merlin (fun pipeline ->
    Mpipeline.reader_comments pipeline |> List.exists ~f:loc_contains_position)
;;
