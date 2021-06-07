open Import

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

type t =
  { stdin : Scheduler.thread Lazy_fiber.t
  ; stderr : Scheduler.thread Lazy_fiber.t
  ; stdout : Scheduler.thread Lazy_fiber.t
  }

let create () =
  let stdout = Lazy_fiber.create Scheduler.create_thread in
  let stderr = Lazy_fiber.create Scheduler.create_thread in
  let stdin = Lazy_fiber.create Scheduler.create_thread in
  { stdout; stderr; stdin }

let run_command state bin stdin_value args : command_result Fiber.t =
  let open Fiber.O in
  let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
  let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
  let stderr_i, stderr_o = Unix.pipe ~cloexec:true () in
  let pid =
    let args = Array.of_list (bin :: args) in
    Unix.create_process bin args stdin_i stdout_o stderr_o |> Stdune.Pid.of_int
  in
  Unix.close stdin_i;
  Unix.close stdout_o;
  Unix.close stderr_o;
  let stdin () =
    let+ res =
      let* thread = Lazy_fiber.force state.stdin in
      Scheduler.async_exn thread (fun () ->
          let out_chan = Unix.out_channel_of_descr stdin_o in
          output_string out_chan stdin_value;
          flush out_chan;
          close_out out_chan)
      |> Scheduler.await_no_cancel
    in
    match res with
    | Ok s -> s
    | Error e -> Exn_with_backtrace.reraise e
  in
  let read th from =
    let+ res =
      Scheduler.async_exn th (fun () ->
          let in_ = Unix.in_channel_of_descr from in
          let contents = Stdune.Io.read_all in_ in
          close_in_noerr in_;
          contents)
      |> Scheduler.await_no_cancel
    in
    match res with
    | Ok s -> s
    | Error e -> Exn_with_backtrace.reraise e
  in
  let stdout () =
    let* th = Lazy_fiber.force state.stdout in
    read th stdout_i
  in
  let stderr () =
    let* th = Lazy_fiber.force state.stderr in
    read th stderr_i
  in
  let+ status, (stdout, stderr) =
    Fiber.fork_and_join
      (fun () -> Scheduler.wait_for_process pid)
      (fun () ->
        Fiber.fork_and_join_unit stdin (fun () ->
            Fiber.fork_and_join stdout stderr))
  in
  { stdout; stderr; status }

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

let message = function
  | Unsupported_syntax syntax ->
    sprintf "formatting %s files is not supported"
      (Document.Syntax.human_name syntax)
  | Missing_binary { binary } ->
    sprintf
      "Unable to find %s binary. You need to install %s manually to use the \
       formatting feature."
      binary binary
  | Unknown_extension uri ->
    Printf.sprintf "Unable to format. File %s has an unknown extension"
      (Uri.to_path uri)
  | Unexpected_result { message } -> message

type formatter =
  | Reason of Document.Kind.t
  | Ocaml of Uri.t

let args = function
  | Ocaml uri -> [ sprintf "--name=%s" (Uri.to_path uri); "-" ]
  | Reason kind -> (
    [ "--parse"; "re"; "--print"; "re" ]
    @
    match kind with
    | Impl -> []
    | Intf -> [ "--interface=true" ])

let binary_name t =
  match t with
  | Ocaml _ -> "ocamlformat"
  | Reason _ -> "refmt"

let binary t =
  let name = binary_name t in
  match Bin.which name with
  | None -> Result.Error (Missing_binary { binary = name })
  | Some b -> Ok b

let formatter doc =
  match Document.syntax doc with
  | (Ocamllex | Menhir) as s -> Error (Unsupported_syntax s)
  | Ocaml -> Ok (Ocaml (Document.uri doc))
  | Reason -> Ok (Reason (Document.kind doc))

let exec state bin args stdin =
  let refmt = Fpath.to_string bin in
  let open Fiber.O in
  let+ res = run_command state refmt stdin args in
  match res.status with
  | Unix.WEXITED 0 -> Result.Ok res.stdout
  | _ -> Result.Error (Unexpected_result { message = res.stderr })

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
      old_lines
      |> Array.fold_left ~init:(0, String.Map.empty) ~f:(fun (i, m) line ->
             ( i + 1
             , String.Map.update
                 ~f:(function
                   | None -> Some [ i ]
                   | Some xs -> Some (i :: xs))
                 m line ))
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
        old_indices
        |> List.iter ~f:(fun iold ->
               let o =
                 1
                 + (Int.Map.find !overlap (iold - 1) |> Option.value ~default:0)
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
          get_diff' old_lines_presubseq new_lines_presubseq
          @ [ Equal unchanged_lines ]
          @ get_diff' old_lines_postsubseq new_lines_postsubseq
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

let diff_edits (orig : string) (formatted : string) : TextEdit.t list =
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

let run state doc : (TextEdit.t list, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = formatter doc in
    let args = args formatter in
    let+ binary = binary formatter in
    (binary, args, Document.source doc |> Msource.text)
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, args, contents) ->
    exec state binary args contents
    |> Fiber.map ~f:(Result.map ~f:(diff_edits contents))
