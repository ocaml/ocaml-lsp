open! Import

module Grammar: sig
  type var =
    | TM_SELECTED_TEXT
    | TM_CURRENT_LINE
    | TM_CURRENT_WORD
    | TM_LINE_INDEX
    | TM_LINE_NUMBER
    | TM_FILENAME
    | TM_FILENAME_BASE
    | TM_DIRECTORY
    | TM_FILEPATH

  type variable_transform = {
    regex: string;
    format_string: string;
    regex_options: string option
  }

  type t

  val tabstop: int -> t
  val placeholder: int -> t -> t
  val choice: int -> string list -> t
  val variable: ?opt:[`Placeholder of t | `Transform of variable_transform | `None] -> var -> t
  val variable_transform: regex:string -> ?regex_options:string -> format_string:string -> variable_transform
  val text: string -> t
end

type t = Grammar.t

val to_string: t -> string