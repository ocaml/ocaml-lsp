open Import

let read_to_end (in_chan : in_channel) : string =
  let buf = Buffer.create 0 in
  let chunk_size = 1024 in
  let chunk = Bytes.create chunk_size in
  let rec go pos =
    let actual_len = input in_chan chunk pos chunk_size in
    if actual_len > 0 then (
      Buffer.add_subbytes buf chunk 0 actual_len;
      go pos
    )
  in
  go 0;
  Buffer.contents buf

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

let run_command command ?stdin_value args : command_result =
  let command =
    match args with
    | [] -> command
    | _ -> Printf.sprintf "%s %s" command (String.concat ~sep:" " args)
  in
  let env = Unix.environment () in
  let in_chan, out_chan, err_chan = Unix.open_process_full command env in
  let f stdin =
    output_string out_chan stdin;
    flush out_chan;
    close_out out_chan
  in
  Option.iter stdin_value ~f;
  let stdout = read_to_end in_chan in
  let stderr = read_to_end err_chan in
  let status = Unix.close_process_full (in_chan, out_chan, err_chan) in
  { stdout; stderr; status }

let _PATH =
  lazy
    (Bin.parse_path
       (Option.value ~default:"" (Unix_env.get Unix_env.initial "PATH")))

type error =
  | Missing_binary
  | Message of string
