open Import

module File: sig
  type t = Impl | Intf | Name of string
end

module Input: sig
  type t =
    | Stdin of string * File.t
    | File of string
end

module Output: sig
  type _ t =
    | File: string -> unit t
    | Stdout: string t
end

module Config: sig
  type t = (string * string) list
  val to_comma_separated_list: t -> string
  val of_comma_separated_list: string -> t option
end

module Options: sig
  module Profile: sig
    type t = Conventional|Default|Compact|Sparse|Ocamlformat|Janestreet
  end
  type t =
    {
      config: Config.t option;
      commentCheck: bool;
      disableConfAttrs: bool;
      disableConfLines: bool;
      enableOutsideDetectedProject: bool;
      ignoreInvalidOption: bool;
      maxIters: int;
      noCommentCheck: bool;
      noVersionCheck: bool;
      ocpIndentConfig: bool;
      profile: Profile.t;
      quiet: bool;
      root: string option;
    }
  val default: t
end

type 'result command =
  | FormatFile: Input.t * 'result Output.t -> 'result command
  | FormatFilesInPlace: string list -> unit command
  | Check: Input.t -> bool command

val exec:
     'result command
  -> Options.t
  -> ('result, string) Result.t

val getConfig:
      _ command
  -> Options.t
  -> (Config.t, string) Result.t

val formatFile:
     Input.t
  -> 'result Output.t
  -> Options.t
  -> ('result, string) Result.t

val formatFilesInPlace:
     string list
  -> Options.t
  -> (unit, string) Result.t

val check:
     Input.t
  -> Options.t
  -> (bool, string) Result.t
