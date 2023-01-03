open Import
module TextEdit = Types.TextEdit
module Position = Types.Position
module Range = Types.Range

module Simple_diff = struct
  (* based on *)
  (* https://github.com/paulgb/simplediff/blob/031dc772ca6795cfdfed27384a6b79e772213233/python/simplediff/__init__.py *)

  type item = string

  type diff =
    | Deleted of item Array_view.t
    | Added of item Array_view.t
    | Equal of item Array_view.t

  let line_map old_lines new_lines =
    let _, map =
      Array.fold_left
        old_lines
        ~init:(0, String.Map.empty)
        ~f:(fun (i, m) line ->
          ( i + 1
          , String.Map.update m ~key:line ~f:(function
                | None -> Some [ i ]
                | Some xs -> Some (i :: xs)) ))
    in
    Array.map new_lines ~f:(fun x ->
        String.Map.find_opt x map |> Option.value ~default:[])

  let longest_subsequence (map : int list array) old_lines new_lines =
    let overlap = ref Int.Map.empty in

    let sub_start_old = ref 0 in
    let sub_start_new = ref 0 in
    let sub_length = ref 0 in

    let old_lines_pos = Array_view.backing_array_pos old_lines 0 in
    let old_len = Array_view.length old_lines in
    Array_view.iteri new_lines ~f:(fun inew _v ->
        let overlap' = ref Int.Map.empty in
        (* where does the new line appear in the old text *)
        let old_indices = map.(Array_view.backing_array_pos new_lines inew) in
        List.iter old_indices ~f:(fun iold ->
            let iold = iold - old_lines_pos in
            if iold >= 0 && iold < old_len then (
              let o =
                1
                + (Int.Map.find_opt (iold - 1) !overlap
                  |> Option.value ~default:0)
              in
              overlap' := Int.Map.add !overlap' ~key:iold ~data:o;

              if o > !sub_length then (
                sub_length := o;
                sub_start_old := iold - o + 1;
                sub_start_new := inew - o + 1)));

        overlap := !overlap');

    (!sub_start_new, !sub_start_old, !sub_length)

  let get_diff old_lines new_lines =
    let old_lines = Array.of_list old_lines in
    let new_lines = Array.of_list new_lines in
    let map = line_map old_lines new_lines in
    let rec get_diff' old_lines new_lines =
      match (Array_view.is_empty old_lines, Array_view.is_empty new_lines) with
      | true, true -> []
      | false, true -> [ Deleted old_lines ]
      | true, false -> [ Added new_lines ]
      | false, false ->
        let sub_start_new, sub_start_old, sub_length =
          longest_subsequence map old_lines new_lines
        in
        if sub_length = 0 then [ Deleted old_lines; Added new_lines ]
        else
          let old_lines_presubseq =
            Array_view.sub ~pos:0 ~len:sub_start_old old_lines
          in
          let new_lines_presubseq =
            Array_view.sub ~pos:0 ~len:sub_start_new new_lines
          in
          let old_lines_postsubseq =
            let start_index = sub_start_old + sub_length in
            let len = Array_view.length old_lines - start_index in
            Array_view.sub ~pos:start_index ~len old_lines
          in
          let new_lines_postsubseq =
            let start_index = sub_start_new + sub_length in
            let len = Array_view.length new_lines - start_index in
            Array_view.sub ~pos:start_index ~len new_lines
          in
          let unchanged_lines =
            Array_view.sub ~pos:sub_start_new ~len:sub_length new_lines
          in
          List.concat
            [ get_diff' old_lines_presubseq new_lines_presubseq
            ; [ Equal unchanged_lines ]
            ; get_diff' old_lines_postsubseq new_lines_postsubseq
            ]
    in
    let make a = Array_view.make ~pos:0 a in
    get_diff' (make old_lines) (make new_lines)
end

type edit =
  | Insert of string array
  | Replace of string array * string array
  | Delete of string array

let text_edit ~line edit =
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
    | Some adds -> adds |> Array.to_list |> String.concat ~sep:""
  in
  { TextEdit.newText; range }

let split_lines =
  let rec loop acc s len i =
    if i >= len then acc
    else
      match String.index_from_opt s i '\n' with
      | None -> String.sub s ~pos:i ~len:(len - i) :: acc
      | Some j ->
        let acc = String.sub s ~pos:i ~len:(j - i + 1) :: acc in
        loop acc s len (j + 1)
  in
  fun s -> List.rev @@ loop [] s (String.length s) 0

let edit ~from:orig ~to_:formatted : TextEdit.t list =
  let line, prev_deleted_lines, edits_rev =
    let orig_lines = split_lines orig in
    let formatted_lines = split_lines formatted in
    Simple_diff.get_diff orig_lines formatted_lines
    |> List.fold_left
         ~init:(0, [||], [])
         ~f:(fun (line, prev_deleted_lines, edits_rev) edit ->
           match (edit : Simple_diff.diff) with
           | Deleted deleted_lines ->
             let new_prev_deleted_lines =
               Array.make
                 (Array.length prev_deleted_lines
                 + Array_view.length deleted_lines)
                 ""
             in
             Array.blit
               ~src:prev_deleted_lines
               ~src_pos:0
               ~dst:new_prev_deleted_lines
               ~dst_pos:0
               ~len:(Array.length prev_deleted_lines);
             Array_view.blit
               deleted_lines
               new_prev_deleted_lines
               ~pos:(Array.length prev_deleted_lines);
             (line, new_prev_deleted_lines, edits_rev)
           | Added added_lines ->
             let added_lines = Array_view.copy added_lines in
             let edit =
               text_edit
                 ~line
                 (if Array.length prev_deleted_lines > 0 then
                  Replace (prev_deleted_lines, added_lines)
                 else Insert added_lines)
             in
             let line = line + Array.length prev_deleted_lines in
             (line, [||], edit :: edits_rev)
           | Equal equal_lines ->
             let edits_rev =
               if Array.length prev_deleted_lines > 0 then
                 text_edit ~line (Delete prev_deleted_lines) :: edits_rev
               else edits_rev
             in
             let line =
               line
               + Array.length prev_deleted_lines
               + Array_view.length equal_lines
             in
             (line, [||], edits_rev))
  in
  List.rev
  @@
  if Array.length prev_deleted_lines > 0 then
    text_edit ~line (Delete prev_deleted_lines) :: edits_rev
  else edits_rev
