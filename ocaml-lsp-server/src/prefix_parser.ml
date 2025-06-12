open Import

include struct
  open Re

  (* Regex based parser *)
  let white_space = set "\n\t "
  let name_char = Re.alt [ rg 'a' 'z'; rg 'A' 'Z'; rg '0' '9'; char '_'; char '\'' ]

  let name_with_dot =
    Re.seq [ name_char; white_space |> rep; char '.'; white_space |> rep ]
  ;;

  let core_operator_str = {|$&*+-/=>@^||}
  let operator = core_operator_str ^ {|~!?%<:.|}
  let infix = set (operator ^ "#")

  let name_or_label =
    compile
      (seq
         [ alt [ set "~?``"; str "let%"; str "and%" ] |> opt
         ; alt [ name_char; name_with_dot ] |> rep1
         ; stop
         ])
  ;;

  (** matches let%lwt and let* style expressions. See
      here:https://v2.ocaml.org/manual/bindingops.html *)
  let monadic_bind =
    compile
      (seq
         [ alt [ str "let"; str "and" ]
         ; alt [ infix |> rep1; seq [ name_char |> rep1; char '%' ] ]
         ; stop
         ])
  ;;

  let infix_operator = compile (seq [ infix |> rep1; stop ])
end

let parse ~pos ~len text =
  (*Attempt to match each of our possible prefix types, the order is important
    because there is some overlap between the regexs*)
  let matched =
    List.find_map [ name_or_label; monadic_bind; infix_operator ] ~f:(fun regex ->
      Re.exec_opt ~pos ~len regex text)
  in
  matched |> Option.map ~f:(fun x -> Re.Group.get x 0)
;;
