open Import

module Options = struct
  type t = (string * string option) list

  let to_string_array (opts: t) : string array =
    List.map opts ~f:(fun (k, vo) ->
      let buf = Buffer.create 0 in
      let bar, sep =
        if String.length k = 1 then "-", " "
        else "--", "="
      in
      begin
        Buffer.add_string buf bar; Buffer.add_string buf k;
        match vo with
        | None -> ()
        | Some v -> begin
          Buffer.add_string buf sep; Buffer.add_string buf v
        end;
      end;
      Buffer.contents buf
    ) |> Array.of_list
end

let read_to_end ?(hint=0) (inChan: in_channel) : string =
  let buf = Buffer.create hint in
  let rec go () =
    try
      let line = input_line inChan in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
      go ()
    with End_of_file ->
      Buffer.contents buf
  in go ()

let run_command command args input : (string, string) Result.t =
  let inChan, outChan, errChan = Unix.open_process_args_full command args [||] in
  output_string outChan input;
  close_out outChan;
  let result = read_to_end ~hint:(String.length input) inChan in
  let error = read_to_end errChan in
  match Unix.close_process_full (inChan, outChan, errChan) with
  | Unix.WEXITED 0 -> Result.Ok result
  | _ -> Result.Error error

let exec (src: string) (opts: Options.t) : (string, string) Result.t =
  let default_opts = [|
    "--quiet";
    (* read from stdin *) "-"
  |] in
  run_command "ocamlformat" (Array.append (Options.to_string_array opts) default_opts) src
