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
  | FormatFile : Input.t * 'result Output.t -> 'result command
  | FormatFilesInPlace : string list -> unit command
  | Check : Input.t -> bool command

val exec : 'result command -> Options.t -> ('result, string) Result.t

val format_file :
  Input.t -> 'result Output.t -> Options.t -> ('result, string) Result.t

val format_files_in_place : string list -> Options.t -> (unit, string) Result.t

val check : Input.t -> Options.t -> (bool, string) Result.t
