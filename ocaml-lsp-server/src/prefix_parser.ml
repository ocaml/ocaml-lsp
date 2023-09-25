open Re

(*Regex based parser*)

let name_char =
  Re.alt [ rg 'a' 'z'; rg 'A' 'Z'; rg '0' '9'; char '_'; char '\'' ]

let name_with_dot =
  Re.seq [ char ' ' |> rep; char '.'; char ' ' |> rep; name_char ]

let core_operator_str = {|$&*+-/=>@^||}

let operator = core_operator_str ^ {|~!?%<:.|}

let infix = set (operator ^ "#")

let name_or_label_regex_rev_2 =
  compile
    (seq
       [ start
       ; alt [ name_char; name_with_dot ] |> rep1
       ; alt [ set "~?``"; str "%tel"; str "%dna" ] |> opt
       ])

(** matches let%lwt and let* style expressions. See
    here:https://v2.ocaml.org/manual/bindingops.html *)
let monadic_bind_rev_2 =
  compile
    (seq
       [ start
       ; alt [ infix |> rep1; seq [ name_char |> rep1; char '%' ] ]
       ; alt [ str "tel"; str "dna" ]
       ])

let infix_regex_rev_2 = compile (seq [ start; infix |> rep1 ])

open Import

module Option = struct
  include Option

  let none_bind func option =
    match option with
    | None -> func ()
    | Some x -> Some x
end

let try_parse_with_regex text =
  let matched =
    Re.exec_opt name_or_label_regex_rev_2 text
    |> Option.none_bind (fun () -> Re.exec_opt monadic_bind_rev_2 text)
    |> Option.none_bind (fun () -> Re.exec_opt infix_regex_rev_2 text)
  in
  matched |> Option.map ~f:(fun x -> Group.get x 0)
