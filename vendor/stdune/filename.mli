include module type of struct
  include Stdlib.Filename
end

val split_extension : string -> string * string

val split_extension_after_dot : string -> string * string

type program_name_kind =
  | In_path
  | Relative_to_current_dir
  | Absolute

val analyze_program_name : string -> program_name_kind
