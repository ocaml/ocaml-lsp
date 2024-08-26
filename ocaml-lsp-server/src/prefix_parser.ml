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

  (*  When completing module paths or record fields Merlin expects the
      beginning of the path and the `.` to be part of the prefix. But when
      accessing an object's methods, the prefix should not contain the `#`
      sign. We use a sub-matching group to that effect.

      - Prefix for [my_record.|] is ["my_record."] (handled by [name_or_label])
      - Prefix for [my_object#|] is [""] (handled by [method_call]) *)
  let method_call = compile (seq [ char '#'; Re.group (rep name_char); stop ])

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
    List.find_map
      [ name_or_label; method_call; monadic_bind; infix_operator ]
      ~f:(fun regex -> Re.exec_opt ~pos ~len regex text)
  in
  matched
  |> Option.map ~f:(fun x ->
    if Re.Group.test x 1 then Re.Group.get x 1 else Re.Group.get x 0)
;;
