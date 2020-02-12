open Import

module FileType = struct
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
    | Stdin of string * FileType.t
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

module Config = struct
  type t = (string * string) list

  let to_comma_separated_list (conf : t) : string =
    List.map conf ~f:(fun (k, v) -> Printf.sprintf "%s=%s" k v)
    |> String.concat ~sep:","

  let of_comma_separated_list (s : string) : t option =
    String.split_on_char s ~sep:','
    |> List.fold_right ~init:(Some []) ~f:(fun s state ->
           match state with
           | None -> None
           | Some acc -> (
             match String.split_on_char s ~sep:'=' with
             | k :: v :: _ -> Some ((k, v) :: acc)
             | _ -> None ))

  let of_print_config_output (s : string) : t option =
    String.split_lines s
    |> List.fold_right ~init:(Some []) ~f:(fun s state ->
           match state with
           | None -> None
           | Some acc -> (
             match String.split_on_char s ~sep:' ' with
             | [ x ]
             | x :: "(file" :: _ -> (
               match String.split_on_char x ~sep:'=' with
               | [ key; value ] -> Some ((key, value) :: acc)
               | _ -> None )
             | _ -> None ))
end

let append_if flag value xs =
  if flag then
    value :: xs
  else
    xs

let append_opt opt f xs =
  match opt with
  | None -> xs
  | Some x -> f x :: xs

module Options = struct
  module Profile = struct
    type t =
      | Conventional
      | Default
      | Compact
      | Sparse
      | Ocamlformat
      | Janestreet

    let to_string = function
      | Conventional -> "conventional"
      | Default -> "default"
      | Compact -> "compact"
      | Sparse -> "sparse"
      | Ocamlformat -> "ocamlformat"
      | Janestreet -> "janestreet"
  end

  type t =
    { config : Config.t option
    ; commentCheck : bool option
    ; disableConfAttrs : bool
    ; disableConfLines : bool
    ; enableOutsideDetectedProject : bool
    ; ignoreInvalidOption : bool
    ; maxIters : int option
    ; noVersionCheck : bool
    ; ocpIndentConfig : bool
    ; profile : Profile.t option
    ; quiet : bool option
    ; root : string option
    }

  let default =
    { config = None
    ; commentCheck = None
    ; disableConfAttrs = false
    ; disableConfLines = false
    ; enableOutsideDetectedProject = false
    ; ignoreInvalidOption = false
    ; maxIters = None
    ; noVersionCheck = false
    ; ocpIndentConfig = false
    ; profile = None
    ; quiet = None
    ; root = None
    }

  let to_cmdline_args (opt : t) : string list =
    []
    |> append_opt opt.root (Printf.sprintf "--root=%s")
    |> append_opt opt.quiet (function
         | true -> "--quiet"
         | _ -> "--no-quiet")
    |> append_opt opt.profile Profile.to_string
    |> append_if opt.ocpIndentConfig "--ocp-indent-config"
    |> append_if opt.noVersionCheck "--no-version-check"
    |> append_opt opt.maxIters (Printf.sprintf "--max-iters:%d")
    |> append_if opt.ignoreInvalidOption "--ignore-invalid-option"
    |> append_if opt.enableOutsideDetectedProject
         "--enable-outside-detected-project"
    |> append_if opt.disableConfLines "-disable-conf-lines"
    |> append_if opt.disableConfAttrs "-disable-conf-attrs"
    |> append_opt opt.commentCheck (function
         | true -> "--comment-check"
         | _ -> "--no-comment-check")
    |> append_opt opt.config Config.to_comma_separated_list
end

type 'result command =
  | FormatFile : Input.t * 'result Output.t -> 'result command
  | FormatFilesInPlace : string list -> unit command
  | Check : Input.t -> bool command

let read_to_end ?(hint = 0) (inChan : in_channel) : string =
  let buf = Buffer.create hint in
  let rec go () =
    try
      let line = input_line inChan in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
      go ()
    with End_of_file -> Buffer.contents buf
  in
  go ()

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
  let inChan, outChan, errChan = Unix.open_process_full command env in
  let f stdin =
    output_string outChan stdin;
    close_out outChan
  in
  Option.iter stdin_value ~f;
  let stdout = read_to_end inChan in
  let stderr = read_to_end errChan in
  let status = Unix.close_process_full (inChan, outChan, errChan) in
  { stdout; stderr; status }

let build_args : type r. r command -> Options.t -> string list * string option =
 fun cmd opts ->
  let optArgs = Options.to_cmdline_args opts in
  match cmd with
  | FormatFile (i, o) -> (
    let args = optArgs @ Output.to_cmdline_args o in
    match i with
    | Input.File f -> (args @ [ f ], None)
    | Input.Stdin (v, f) -> (args @ FileType.to_cmdline_args f @ [ "-" ], Some v)
    )
  | FormatFilesInPlace fs -> (optArgs @ ("--inplace" :: fs), None)
  | Check i -> (
    match i with
    | Input.File f -> (optArgs @ [ f ], None)
    | Input.Stdin (v, f) ->
      (optArgs @ FileType.to_cmdline_args f @ [ "-" ], Some v) )

let exec : type r. r command -> Options.t -> (r, string) Result.t =
 fun cmd opts ->
  let args, stdin_value = build_args cmd opts in
  let res = run_command "ocamlformat" ?stdin_value args in
  match res.status with
  | Unix.WEXITED i -> (
    match cmd with
    | Check _ -> Result.Ok (i = 0)
    | _ when i <> 0 -> Result.Error res.stderr
    | FormatFile (_, o) -> (
      match o with
      | Output.File _ -> Result.Ok ()
      | Output.Stdout -> Result.Ok res.stdout )
    | FormatFilesInPlace _ -> Result.Ok () )
  | _ -> Result.Error res.stderr

let get_config : type r. r command -> Options.t -> (Config.t, string) Result.t =
 fun cmd opts ->
  let args, stdin_value = build_args cmd opts in
  let args = "--print-config" :: args in
  let res = run_command "ocamlformat" ?stdin_value args in
  match res.status with
  | Unix.WEXITED 0 -> (
    match Config.of_print_config_output res.stdout with
    | None -> Result.Error "get_config: parse error"
    | Some c -> Result.Ok c )
  | _ -> Result.Error res.stderr

let format_file :
    type r. Input.t -> r Output.t -> Options.t -> (r, string) Result.t =
 fun input output opts -> exec (FormatFile (input, output)) opts

let format_files_in_place (files : string list) (opts : Options.t) :
    (unit, string) Result.t =
  exec (FormatFilesInPlace files) opts

let check (input : Input.t) (opts : Options.t) : (bool, string) Result.t =
  exec (Check input) opts
