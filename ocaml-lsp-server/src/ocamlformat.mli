open Import

module File_type : sig
  type t =
    | Impl
    | Intf
    | Name of string
end

module Input : sig
  type t =
    | Stdin of string * File_type.t
    | File of string
end

module Output : sig
  type _ t =
    | File : string -> unit t
    | Stdout : string t
end

module Options : sig
  type t = string list

  val default : t
end

type 'result command =
  | Format_file : Input.t * 'result Output.t -> 'result command
  | Format_files_in_place : string list -> unit command
  | Check : Input.t -> bool command

type error =
  | Missing_binary
  | Message of string

val exec : 'result command -> Options.t -> ('result, error) Result.t

val format_file :
  Input.t -> 'result Output.t -> Options.t -> ('result, error) Result.t

val format_files_in_place : string list -> Options.t -> (unit, error) Result.t

val check : Input.t -> Options.t -> (bool, error) Result.t
