type parse_state =
  | Continue
  | End
  | IncludeAndEnd
  | Fail

let next continue = if continue then Continue else End

let is_name_body_char char =
  match char with
  | '0' .. '9' | '\'' | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

let parse_name_char ~next_char currentChar =
  match currentChar with
  | '.' -> next_char |> is_name_body_char |> next
  | '`' -> IncludeAndEnd
  | '~' | '?' ->
    if next_char |> is_name_body_char || next_char = ' ' then IncludeAndEnd
    else End
  | c -> c |> is_name_body_char |> next

let is_name_char ~next_char currentChar =
  match parse_name_char ~next_char currentChar with
  | IncludeAndEnd | Continue -> true
  | Fail | End -> false

let is_infix_char' char =
  match char with
  | '~'
  | '?'
  | ':'
  | '!'
  | '$'
  | '&'
  | '*'
  | '+'
  | '-'
  | '/'
  | '='
  | '>'
  | '@'
  | '^'
  | '|'
  | '%'
  | '<'
  | '.'
  | '#' -> true
  | _ -> false

let parse_infix_char ~next_char:_ char = is_infix_char' char |> next

let parse_char is_correct_char text =
  let rec loop text length =
    match text with
    | char :: (next_char :: _ as tail) -> (
      match is_correct_char ~next_char char with
      | Continue -> loop tail (length + 1)
      | IncludeAndEnd -> Some (length + 1)
      | End -> Some length
      | Fail -> None)
    (*This is ugly but i'm not sure how else to deal with reaching the start of
      the string*)
    | [ char ] -> (
      match is_correct_char ~next_char:' ' char with
      | Continue -> Some (length + 1)
      | IncludeAndEnd -> Some (length + 1)
      | End -> Some length
      | Fail -> None)
    | _ -> Some length
  in
  let len = loop text 0 in
  Option.bind len (fun x -> if x = 0 then None else Some x)

let infix_prefix text =
  if text |> List.hd |> is_infix_char' && List.nth text 1 |> is_infix_char' then
    None
  else parse_char parse_infix_char text

let name_prefix = parse_char parse_name_char

let rec try_parse parsers str =
  match parsers with
  | head :: tail -> (
    match head str with
    | Some l -> Some l
    | None -> str |> try_parse tail)
  | [] -> None
open Re

(*Regex based parser*)

let name_or_label_regex =
   Re.compile @@ Re.Posix.re {|([~?`])?([a-zA-Z0-9_']|[a-zA-Z0-9_']\.)+$|}

let infixRegex =Re.compile @@ Re.Posix.re {|[~?:!$&*+\-\/=><@^|%<.#]+$|}

open Import

module Option = struct
  include Option

  let none_bind func option =
    match option with
    | None -> func ()
    | Some x -> Some x
end

let try_parse_regex text =

  let matched =
    Re.exec_opt name_or_label_regex text
    |> Option.none_bind (fun () -> Re.exec_opt infixRegex text)
  in
  matched |>Option.map ~f:(fun x->Group.get x 0)
