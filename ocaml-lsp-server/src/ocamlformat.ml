open Import
open Base_formatter

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

let build_args : type r. r command -> Options.t -> string list * string option =
 fun cmd args ->
  match cmd with
  | Format_file (i, o) -> (
    let output = Output.to_cmdline_args o in
    match i with
    | Input.File f -> (args @ output @ [ f ], None)
    | Input.Stdin (v, f) ->
      (args @ output @ File_type.to_cmdline_args f @ [ "-" ], Some v) )

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
      | _ when i <> 0 -> Result.Error (Message res.stderr)
      | Format_file (_, o) -> (
        match o with
        | Output.File _ -> Result.Ok ()
        | Output.Stdout -> Result.Ok res.stdout ) )
    | _ -> Result.Error (Message res.stderr) )

let format_file :
    type r. Input.t -> r Output.t -> Options.t -> (r, error) Result.t =
 fun input output args -> exec (Format_file (input, output)) args
