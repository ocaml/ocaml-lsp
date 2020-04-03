open! Import

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

type variable_transform =
  { regex : string
  ; format_string : string
  ; regex_options : string option
  }

type t

module Grammar : sig
  val tabstop : int -> t

  val placeholder : ?index:int -> t -> t

  val choice : ?index:int -> string list -> t

  val variable :
       ?opt:[ `Placeholder of t | `Transform of variable_transform | `None ]
    -> var
    -> t

  val variable_transform :
       regex:string
    -> ?regex_options:string
    -> format_string:string
    -> variable_transform

  val text : string -> t

  val ( ^^ ) : t -> t -> t

  val ( @+ ) : string -> t -> t

  val ( +@ ) : t -> string -> t
end

val concat : ?sep:t -> t list -> t

val to_string : t -> string

val pp_t : Format.formatter -> t -> unit
