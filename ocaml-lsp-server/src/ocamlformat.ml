open Import

module File_type = struct
  type t =
    | Impl
    | Intf
    | Name of string

  let to_cmdline_args = function
    | Impl -> [ "--impl" ]
    | Intf -> [ "--intf" ]
    | Name n -> [ Printf.sprintf "--name=%s" n ]
end

module Input = struct
  type t =
    | Stdin of string * File_type.t
    | File of string
end

module Output = struct
  type _ t =
    | File : string -> unit t
    | Stdout : string t

  let to_cmdline_args : type o. o t -> string list = function
    | File s -> [ Printf.sprintf "--output=%s" s ]
    | Stdout -> []
end

module Options = struct
  type t = string list

  let default : t = []
end

type 'result command =
  | Format_file : Input.t * 'result Output.t -> 'result command
  | Format_files_in_place : string list -> unit command
  | Check : Input.t -> bool command

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

let build_args : type r. r command -> Options.t -> string list * string option =
 fun cmd args ->
  match cmd with
  | Format_file (i, o) -> (
    let output = Output.to_cmdline_args o in
    match i with
    | Input.File f -> (args @ output @ [ f ], None)
    | Input.Stdin (v, f) ->
      (args @ output @ File_type.to_cmdline_args f @ [ "-" ], Some v) )
  | Format_files_in_place fs -> (args @ ("--inplace" :: fs), None)
  | Check i -> (
    match i with
    | Input.File f -> (args @ [ f ], None)
    | Input.Stdin (v, f) ->
      (args @ File_type.to_cmdline_args f @ [ "-" ], Some v) )

let _PATH =
  lazy
    (Bin.parse_path
       (Option.value ~default:"" (Unix_env.get Unix_env.initial "PATH")))

type error =
  | Missing_binary
  | Message of string

let exec : type r. r command -> Options.t -> (r, error) Result.t =
 fun cmd args ->
  let args, stdin_value = build_args cmd args in
  let ocamlformat = Bin.which ~path:(Lazy.force _PATH) "ocamlformat" in
  match ocamlformat with
  | None -> Result.Error Missing_binary
  | Some ocamlformat -> (
    let ocamlformat = Fpath.to_string ocamlformat in
    let res = run_command ocamlformat ?stdin_value args in
    match res.status with
    | Unix.WEXITED i -> (
      match cmd with
      | Check _ -> Result.Ok (i = 0)
      | _ when i <> 0 -> Result.Error (Message res.stderr)
      | Format_file (_, o) -> (
        match o with
        | Output.File _ -> Result.Ok ()
        | Output.Stdout -> Result.Ok res.stdout )
      | Format_files_in_place _ -> Result.Ok () )
    | _ -> Result.Error (Message res.stderr) )

let format_file :
    type r. Input.t -> r Output.t -> Options.t -> (r, error) Result.t =
 fun input output args -> exec (Format_file (input, output)) args

let format_files_in_place (files : string list) (options : Options.t) :
    (unit, error) Result.t =
  exec (Format_files_in_place files) options

let check (input : Input.t) (options : Options.t) : (bool, error) Result.t =
  exec (Check input) options
