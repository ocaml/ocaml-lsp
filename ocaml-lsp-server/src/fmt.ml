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

let run_command command stdin_value args : command_result =
  let args = Array.of_list (command :: args) in
  let env = Unix.environment () in
  let in_chan, out_chan, err_chan =
    Unix.open_process_args_full command args env
  in
  output_string out_chan stdin_value;
  flush out_chan;
  close_out out_chan;
  let stdout = read_to_end in_chan in
  let stderr = read_to_end err_chan in
  let status = Unix.close_process_full (in_chan, out_chan, err_chan) in
  { stdout; stderr; status }

let _PATH =
  lazy
    (Bin.parse_path
       (Option.value ~default:"" (Unix_env.get Unix_env.initial "PATH")))

type error =
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of { name : string }

let message = function
  | Missing_binary { binary } -> sprintf "Unable to find %s binary" binary
  | Unknown_extension { name } ->
    Printf.sprintf "Unable to format. File %s has an unknown extension" name
  | Unexpected_result { message } -> message

module Ml_kind = struct
  type t =
    | Intf
    | Impl
end

type formatter =
  | Reason of Ml_kind.t
  | Ocaml of { name : string }

let args = function
  | Ocaml { name } -> [ sprintf "--name=%s" name; "-" ]
  | Reason kind -> (
    [ "--parse"; "re"; "--print"; "re" ]
    @
    match kind with
    | Impl -> []
    | Intf -> [ "--interface=true" ] )

let binary_name t =
  match t with
  | Ocaml _ -> "ocamlformat"
  | Reason _ -> "refmt"

let binary t =
  let name = binary_name t in
  match Bin.which ~path:(Lazy.force _PATH) name with
  | None -> Result.Error (Missing_binary { binary = name })
  | Some b -> Ok b

let formatter fname =
  match Filename.extension fname with
  | ".ml"
  | ".mli" ->
    Ok (Ocaml { name = fname })
  | ".re" -> Ok (Reason Impl)
  | ".rei" -> Ok (Reason Intf)
  | name -> Error (Unknown_extension { name })

let exec bin args stdin =
  let refmt = Fpath.to_string bin in
  let res = run_command refmt stdin args in
  match res.status with
  | Unix.WEXITED 0 -> Result.Ok res.stdout
  | _ -> Result.Error (Unexpected_result { message = res.stderr })

let run ~fname ~contents =
  let open Result.O in
  let* formatter = formatter fname in
  let args = args formatter in
  let* binary = binary formatter in
  exec binary args contents
