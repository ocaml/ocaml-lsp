open Import
open Base_formatter

module File_type = struct
  type t =
    | Impl
    | Intf

  let to_cmdline_args = function
    | Impl -> []
    | Intf -> [ "--interface=true" ]
end

module Input = struct
  type t =
    | Stdin of string * File_type.t
    | File of string
end

module Output = struct
  type _ t = Stdout : string t
end

module Options = struct
  type t = string list

  let default : t = [ "--parse re"; "--print re" ]
end

type 'result command =
  | Format_file : Input.t -> string command
  | Format_files_in_place : string list -> unit command

let build_args : type r. r command -> Options.t -> string list * string option =
 fun cmd args ->
  match cmd with
  | Format_file i -> (
    match i with
    | Input.File f -> (args @ [ f ], None)
    | Input.Stdin (v, f) -> (args @ File_type.to_cmdline_args f, Some v) )
  | Format_files_in_place fs -> (args @ ("--in-place" :: fs), None)

let exec : type r. r command -> Options.t -> (r, error) Result.t =
 fun cmd args ->
  let args, stdin_value = build_args cmd args in
  let refmt = Bin.which ~path:(Lazy.force _PATH) "refmt" in
  match refmt with
  | None -> Result.Error Missing_binary
  | Some refmt -> (
    let refmt = Fpath.to_string refmt in
    let res = run_command refmt ?stdin_value args in
    match res.status with
    | Unix.WEXITED _ -> (
      match cmd with
      | Format_file _ -> Result.Ok res.stdout
      | Format_files_in_place _ -> Result.Ok () )
    | _ -> Result.Error (Message res.stderr) )

let format_file : Input.t -> Options.t -> (string, error) Result.t =
 fun input args -> exec (Format_file input) args

let format_files_in_place (files : string list) (options : Options.t) :
    (unit, error) Result.t =
  exec (Format_files_in_place files) options
