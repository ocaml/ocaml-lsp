open! Import

module Grammar = struct
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

  type t =
    | Tabstop of int * [ `Placeholder of t | `Choice of string list | `None ]
    | Variable of var * [ `Placeholder of t | `Transform of variable_transform | `None ]
    | Text of string

  let tabstop index = Tabstop (index, `None)
  let placeholder index content = Tabstop (index, `Placeholder content)
  let choice index values =
    if List.is_empty values then raise (Invalid_argument "choice must have non empty values")
    else Tabstop (index, `Choice values)
  let variable ?(opt=`None) var = Variable (var, opt)
  let variable_transform ~regex ?regex_options ~format_string =
    { regex; regex_options; format_string }
  let text str = Text str
end

type t = Grammar.t

let escape ?(in_choice=false) str =
  let str =
    str |> String.replace_all ~pattern:"$" ~with_:"\\$"
        |> String.replace_all ~pattern:"}" ~with_:"\\}"
        |> String.replace_all ~pattern:"\\" ~with_:"\\\\"
  in
  if not in_choice then str
  else
    str |> String.replace_all ~pattern:"," ~with_:"\\,"
        |> String.replace_all ~pattern:"|" ~with_:"\\|"

let rec to_string (snippet: t) : string =
  let open Printf in
  match snippet with
  | Text s -> escape s
  | Tabstop (i, `None) -> sprintf "$%d" i
  | Tabstop (i, `Placeholder s) -> sprintf "${%d:%s}" i (to_string s)
  | Tabstop (i, `Choice values) ->
    sprintf "${%d|%s|}" i (values |> List.map ~f:(escape ~in_choice:true) |> String.concat ~sep:",")
  | Variable (var, opt) ->
    let var =
      match var with
      | TM_SELECTED_TEXT -> "TM_SELECTED_TEXT"
      | TM_CURRENT_LINE -> "TM_CURRENT_LINE"
      | TM_CURRENT_WORD -> "TM_CURRENT_WORD"
      | TM_LINE_INDEX -> "TM_LINE_INDEX"
      | TM_LINE_NUMBER -> "TM_LINE_NUMBER"
      | TM_FILENAME -> "TM_FILENAME"
      | TM_FILENAME_BASE -> "TM_FILENAME_BASE"
      | TM_DIRECTORY -> "TM_DIRECTORY"
      | TM_FILEPATH -> "TM_FILEPATH"
    in
    match opt with
    | `None -> sprintf "$%s" var
    | `Placeholder s -> sprintf "${%s:%s}" var (to_string s)
    | `Transform t -> sprintf "${%s/%s/%s/%s}" var t.regex t.format_string (Option.value ~default:"" t.regex_options)
