open Import
open Base_formatter

module File_type : sig
  type t =
    | Impl
    | Intf
end

module Input : sig
  type t =
    | Stdin of string * File_type.t
    | File of string
end

module Output : sig
  type _ t = Stdout : string t
end

module Options : sig
  type t = string list

  val default : t
end

type 'result command =
  | Format_file : Input.t -> string command
  | Format_files_in_place : string list -> unit command

val exec : 'result command -> Options.t -> ('result, error) Result.t

val format_file : Input.t -> Options.t -> (string, error) Result.t

val format_files_in_place : string list -> Options.t -> (unit, error) Result.t
