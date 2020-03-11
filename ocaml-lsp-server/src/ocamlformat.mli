open Import
open Base_formatter

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

val exec : 'result command -> Options.t -> ('result, error) Result.t

val format_file :
  Input.t -> 'result Output.t -> Options.t -> ('result, error) Result.t
