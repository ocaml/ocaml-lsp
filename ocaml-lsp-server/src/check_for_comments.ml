open Import

let position_in_comment ~position ~merlin =
  let position =
    Document.Merlin.to_doc merlin
    |> Document.text_document
    |> fun document -> Text_document.absolute_position document position
  in
  Document.Merlin.with_pipeline_exn ~name:"get-comments" merlin (fun pipeline ->
    Mpipeline.reader_comments pipeline
    |> List.exists ~f:(fun (_, (loc : Loc.t)) ->
      loc.loc_start.pos_cnum <= position && position <= loc.loc_end.pos_cnum))
;;
