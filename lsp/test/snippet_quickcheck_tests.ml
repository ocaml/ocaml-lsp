open Base
open Base_quickcheck
module Snippet = Lsp.Snippet

type text_atom =
  | Letter
  | Space
  | Dollar
  | Close_brace
  | Backslash
  | Comma
  | Pipe
  | Slash
  | Newline
  | Unicode
[@@deriving quickcheck, sexp_of]

type key =
  | Final
  | First
  | Second
  | Third
  | Negative
[@@deriving quickcheck, sexp_of]

let equal_key left right =
  match left, right with
  | Final, Final | First, First | Second, Second | Third, Third | Negative, Negative ->
    true
  | (Final | First | Second | Third | Negative), _ -> false
;;

type variable =
  | Selected_text
  | Current_line
  | Current_word
  | Line_index
  | Line_number
  | Filename
  | Filename_base
  | Directory
  | Filepath
[@@deriving quickcheck, sexp_of]

type transform =
  | Uppercase
  | Global_replace
[@@deriving quickcheck, sexp_of]

type model =
  | Text of text_atom list
  | Tabstop of key
  | Placeholder of key option * model
  | Choice of key option * text_atom list * text_atom list list
  | Variable of variable * variable_option
  | Concat of model * model
  | Concat_many of model list
  | Concat_separated of model list * model
  | Prefix_text of text_atom list * model
  | Suffix_text of model * text_atom list

and variable_option =
  | Variable_none
  | Variable_placeholder of model
  | Variable_transform of transform
[@@deriving quickcheck, sexp_of]

let text_atom = function
  | Letter -> "a"
  | Space -> " "
  | Dollar -> "$"
  | Close_brace -> "}"
  | Backslash -> "\\"
  | Comma -> ","
  | Pipe -> "|"
  | Slash -> "/"
  | Newline -> "\n"
  | Unicode -> "😀"
;;

let text atoms = List.map atoms ~f:text_atom |> String.concat ~sep:""

let key = function
  | Final -> 0
  | First -> 1
  | Second -> 2
  | Third -> 3
  | Negative -> -1
;;

let variable = function
  | Selected_text -> Snippet.Var.TM_SELECTED_TEXT
  | Current_line -> TM_CURRENT_LINE
  | Current_word -> TM_CURRENT_WORD
  | Line_index -> TM_LINE_INDEX
  | Line_number -> TM_LINE_NUMBER
  | Filename -> TM_FILENAME
  | Filename_base -> TM_FILENAME_BASE
  | Directory -> TM_DIRECTORY
  | Filepath -> TM_FILEPATH
;;

let transform = function
  | Uppercase -> Snippet.variable_transform ~regex:"(.*)" ~format_string:"${1:/upcase}" ()
  | Global_replace ->
    Snippet.variable_transform ~regex:"a" ~format_string:"b" ~regex_options:"g" ()
;;

let rec build = function
  | Text atoms -> Snippet.text (text atoms)
  | Tabstop index -> Snippet.tabstop (key index)
  | Placeholder (None, contents) -> Snippet.placeholder (build contents)
  | Placeholder (Some index, contents) ->
    Snippet.placeholder ~index:(key index) (build contents)
  | Choice (index, first, rest) ->
    let values = List.map (first :: rest) ~f:text in
    (match index with
     | None -> Snippet.choice values
     | Some index -> Snippet.choice ~index:(key index) values)
  | Variable (var, Variable_none) -> Snippet.variable (variable var)
  | Variable (var, Variable_placeholder contents) ->
    Snippet.variable (variable var) ~opt:(`Placeholder (build contents))
  | Variable (var, Variable_transform value) ->
    Snippet.variable (variable var) ~opt:(`Transform (transform value))
  | Concat (left, right) ->
    let open Snippet.O in
    build left ^^ build right
  | Concat_many snippets -> Snippet.concat (List.map snippets ~f:build)
  | Concat_separated (snippets, separator) ->
    Snippet.concat ~sep:(build separator) (List.map snippets ~f:build)
  | Prefix_text (prefix, snippet) ->
    let open Snippet.O in
    text prefix @+ build snippet
  | Suffix_text (snippet, suffix) ->
    let open Snippet.O in
    build snippet +@ text suffix
;;

let add_escaped buffer ~in_choice string =
  String.iter string ~f:(function
    | '\\' -> Buffer.add_string buffer "\\\\"
    | '$' -> Buffer.add_string buffer "\\$"
    | '}' -> Buffer.add_string buffer "\\}"
    | ',' when in_choice -> Buffer.add_string buffer "\\,"
    | '|' when in_choice -> Buffer.add_string buffer "\\|"
    | char -> Buffer.add_char buffer char)
;;

let variable_name = function
  | Selected_text -> "TM_SELECTED_TEXT"
  | Current_line -> "TM_CURRENT_LINE"
  | Current_word -> "TM_CURRENT_WORD"
  | Line_index -> "TM_LINE_INDEX"
  | Line_number -> "TM_LINE_NUMBER"
  | Filename -> "TM_FILENAME"
  | Filename_base -> "TM_FILENAME_BASE"
  | Directory -> "TM_DIRECTORY"
  | Filepath -> "TM_FILEPATH"
;;

let reference model =
  let buffer = Buffer.create 32 in
  let next = ref 1 in
  let indices = ref [] in
  let index = function
    | None ->
      let result = !next in
      Int.incr next;
      result
    | Some Final -> 0
    | Some key ->
      (match List.Assoc.find !indices key ~equal:equal_key with
       | Some result -> result
       | None ->
         let result = !next in
         Int.incr next;
         indices := (key, result) :: !indices;
         result)
  in
  let rec render = function
    | Text atoms -> add_escaped buffer ~in_choice:false (text atoms)
    | Tabstop key -> Printf.bprintf buffer "$%d" (index (Some key))
    | Placeholder (key, contents) ->
      Printf.bprintf buffer "${%d:" (index key);
      render contents;
      Buffer.add_char buffer '}'
    | Choice (key, first, rest) ->
      Printf.bprintf buffer "${%d|" (index key);
      List.iteri (first :: rest) ~f:(fun choice_index atoms ->
        if choice_index > 0 then Buffer.add_char buffer ',';
        add_escaped buffer ~in_choice:true (text atoms));
      Buffer.add_string buffer "|}"
    | Variable (var, Variable_none) -> Printf.bprintf buffer "$%s" (variable_name var)
    | Variable (var, Variable_placeholder contents) ->
      Printf.bprintf buffer "${%s:" (variable_name var);
      render contents;
      Buffer.add_char buffer '}'
    | Variable (var, Variable_transform Uppercase) ->
      Printf.bprintf buffer "${%s/(.*)/${1:/upcase}/}" (variable_name var)
    | Variable (var, Variable_transform Global_replace) ->
      Printf.bprintf buffer "${%s/a/b/g}" (variable_name var)
    | Concat (left, right) ->
      render left;
      render right
    | Concat_many snippets -> List.iter snippets ~f:render
    | Concat_separated (snippets, separator) ->
      List.iteri snippets ~f:(fun snippet_index snippet ->
        if snippet_index > 0 then render separator;
        render snippet)
    | Prefix_text (prefix, snippet) ->
      render (Text prefix);
      render snippet
    | Suffix_text (snippet, suffix) ->
      render snippet;
      render (Text suffix)
  in
  render model;
  Buffer.contents buffer
;;

module Case = struct
  type t = model [@@deriving quickcheck, sexp_of]
end

let examples =
  [ Text [ Dollar; Close_brace; Backslash; Comma; Pipe ]
  ; Choice (None, [ Dollar ], [ [ Close_brace ]; [ Backslash ]; [ Comma ]; [ Pipe ] ])
  ; Tabstop Final
  ; Concat
      ( Placeholder (Some First, Text [ Letter ])
      , Concat
          ( Placeholder (Some First, Text [ Unicode ])
          , Variable
              (Filename, Variable_placeholder (Placeholder (None, Text [ Newline ]))) ) )
  ]
;;

let check model =
  let snippet = build model in
  let expected = reference model in
  let actual = Snippet.to_string snippet in
  let pretty_printed = Stdlib.Format.asprintf "%a" Snippet.pp snippet in
  if not (String.equal expected actual)
  then failwith (Printf.sprintf "to_string: expected %S, got %S" expected actual);
  if not (String.equal expected pretty_printed)
  then failwith (Printf.sprintf "pp: expected %S, got %S" expected pretty_printed)
;;

let%test_unit "snippet rendering agrees with the reference implementation" =
  Test.run_exn (module Case) ~examples ~f:check
;;
