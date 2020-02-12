open Import

module FileType : sig
  type t =
    | Impl
    | Intf
    | Name of string
end

module Input : sig
  type t =
    | Stdin of string * FileType.t
    | File of string
end

module Output : sig
  type _ t =
    | File : string -> unit t
    | Stdout : string t
end

module Config : sig
  type t = (string * string) list

  val to_comma_separated_list : t -> string

  val of_comma_separated_list : string -> t option
end

module Options : sig
  module Profile : sig
    type t =
      | Conventional
      | Default
      | Compact
      | Sparse
      | Ocamlformat
      | Janestreet
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

  val default : t
end

type 'result command =
  | FormatFile : Input.t * 'result Output.t -> 'result command
  | FormatFilesInPlace : string list -> unit command
  | Check : Input.t -> bool command

val exec : 'result command -> Options.t -> ('result, string) Result.t

val get_config : _ command -> Options.t -> (Config.t, string) Result.t

val format_file :
  Input.t -> 'result Output.t -> Options.t -> ('result, string) Result.t

val format_files_in_place : string list -> Options.t -> (unit, string) Result.t

val check : Input.t -> Options.t -> (bool, string) Result.t
