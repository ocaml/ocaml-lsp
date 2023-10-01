open Re

(*Regex based parser*)
let whiteSpace = set "\n\t "

let name_char =
  Re.alt [ rg 'a' 'z'; rg 'A' 'Z'; rg '0' '9'; char '_'; char '\'' ]

let name_with_dot =
  Re.seq [ whiteSpace |> rep; char '.'; whiteSpace |> rep; name_char ]

let core_operator_str = {|$&*+-/=>@^||}

let operator = core_operator_str ^ {|~!?%<:.|}

let infix = set (operator ^ "#")

let name_or_label =
  compile
    (seq
       [ start
       ; alt [ name_char; name_with_dot ] |> rep1
       ; alt [ set "~?``"; str "%tel"; str "%dna" ] |> opt
       ])

(** matches let%lwt and let* style expressions. See
    here:https://v2.ocaml.org/manual/bindingops.html *)
let monadic_bind =
  compile
    (seq
       [ start
       ; alt [ infix |> rep1; seq [ name_char |> rep1; char '%' ] ]
       ; alt [ str "tel"; str "dna" ]
       ])

let infix_operator = compile (seq [ start; infix |> rep1 ])

open Import

let try_parse_with_regex text =
  (*Attempt to match each of our possible prefix types, the order is important
    because there is some overlap between the regexs*)
  let matched =
    List.find_map
      [ name_or_label; monadic_bind; infix_operator ]
      ~f:(fun regex -> Re.exec_opt regex text)
  in
  matched |> Option.map ~f:(fun x -> Group.get x 0)
