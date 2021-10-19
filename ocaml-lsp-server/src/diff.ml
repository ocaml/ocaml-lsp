open Import

module Simple_diff = struct
  (* based on *)
  (* https://github.com/paulgb/simplediff/blob/031dc772ca6795cfdfed27384a6b79e772213233/python/simplediff/__init__.py *)

  type item = string

  type diff =
    | Deleted of item array
    | Added of item array
    | Equal of item array

  let longest_subsequence old_lines new_lines =
    let _, old_index_map =
      Array.fold_left old_lines ~init:(0, String.Map.empty)
        ~f:(fun (i, m) line ->
          ( i + 1
          , String.Map.update m line ~f:(function
              | None -> Some [ i ]
              | Some xs -> Some (i :: xs)) ))
    in
    let overlap = ref Int.Map.empty in

    let sub_start_old = ref 0 in
    let sub_start_new = ref 0 in
    let sub_length = ref 0 in

    Array.iteri new_lines ~f:(fun inew v ->
        let overlap' = ref Int.Map.empty in
        let old_indices =
          String.Map.find old_index_map v |> Option.value ~default:[]
        in
        List.iter old_indices ~f:(fun iold ->
            let o =
              1 + (Int.Map.find !overlap (iold - 1) |> Option.value ~default:0)
            in
            overlap' := Int.Map.set !overlap' iold o;

            if o > !sub_length then (
              sub_length := o;
              sub_start_old := iold - o + 1;
              sub_start_new := inew - o + 1
            ));

        overlap := !overlap');

    (!sub_start_new, !sub_start_old, !sub_length)

  let get_diff old_lines new_lines =
    let rec get_diff' old_lines new_lines =
      match (old_lines, new_lines) with
      | [||], [||] -> []
      | old_lines, [||] -> [ Deleted old_lines ]
      | [||], new_lines -> [ Added new_lines ]
      | _, _ ->
        let sub_start_new, sub_start_old, sub_length =
          longest_subsequence old_lines new_lines
        in
        if sub_length = 0 then
          [ Deleted old_lines; Added new_lines ]
        else
          let old_lines_presubseq =
            Array.sub ~pos:0 ~len:sub_start_old old_lines
          in
          let new_lines_presubseq =
            Array.sub ~pos:0 ~len:sub_start_new new_lines
          in
          let old_lines_postsubseq =
            let start_index = sub_start_old + sub_length in
            let len = Array.length old_lines - start_index in
            Array.sub ~pos:start_index ~len old_lines
          in
          let new_lines_postsubseq =
            let start_index = sub_start_new + sub_length in
            let len = Array.length new_lines - start_index in
            Array.sub ~pos:start_index ~len new_lines
          in
          let unchanged_lines =
            Array.sub ~pos:sub_start_new ~len:sub_length new_lines
          in
          List.concat
            [ get_diff' old_lines_presubseq new_lines_presubseq
            ; [ Equal unchanged_lines ]
            ; get_diff' old_lines_postsubseq new_lines_postsubseq
            ]
    in
    get_diff' (Array.of_list old_lines) (Array.of_list new_lines)
end

type edit =
  | Insert of string array
  | Replace of string array * string array
  | Delete of string array

let text_edit ~line_sep ~line edit =
  let deleted_lines, added_lines =
    match edit with
    | Insert adds -> (None, Some adds)
    | Replace (dels, adds) -> (Some dels, Some adds)
    | Delete dels -> (Some dels, None)
  in
  let start = { Position.character = 0; line } in
  let end_ =
    { Position.character = 0
    ; line =
        (match deleted_lines with
        | None -> line
        | Some dels -> line + Array.length dels)
    }
  in
  let range = { Range.start; end_ } in
  let newText =
    match added_lines with
    | None -> ""
    | Some adds ->
      (adds |> Array.to_list |> String.concat ~sep:line_sep) ^ line_sep
  in
  { TextEdit.newText; range }

let edit ~from:orig ~to_:formatted : TextEdit.t list =
  let orig_lines = String.split_lines orig in
  let formatted_lines = String.split_lines formatted in
  (* TODO: better way of knowing this ? *)
  let line_sep =
    match String.findi ~f:(fun c -> c = '\r') formatted with
    | Some _ -> "\n\r"
    | None -> "\n"
  in
  let line, prev_deleted_lines, edits_rev =
    Simple_diff.get_diff orig_lines formatted_lines
    |> List.fold_left
         ~init:(0, [||], [])
         ~f:(fun (line, prev_deleted_lines, edits_rev) edit ->
           match (edit : Simple_diff.diff) with
           | Deleted deleted_lines ->
             (line, Array.append prev_deleted_lines deleted_lines, edits_rev)
           | Added added_lines ->
             let edit =
               text_edit ~line_sep ~line
                 (if Array.length prev_deleted_lines > 0 then
                   Replace (prev_deleted_lines, added_lines)
                 else
                   Insert added_lines)
             in
             let line = line + Array.length prev_deleted_lines in
             (line, [||], edit :: edits_rev)
           | Equal equal_lines ->
             let edits_rev =
               if Array.length prev_deleted_lines > 0 then
                 text_edit ~line_sep ~line (Delete prev_deleted_lines)
                 :: edits_rev
               else
                 edits_rev
             in
             let line =
               line + Array.length prev_deleted_lines + Array.length equal_lines
             in
             (line, [||], edits_rev))
  in
  let edits_rev =
    if Array.length prev_deleted_lines > 0 then
      text_edit ~line_sep ~line (Delete prev_deleted_lines) :: edits_rev
    else
      edits_rev
  in
  List.rev edits_rev
