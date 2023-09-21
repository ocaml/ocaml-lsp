open Re

(*Regex based parser*)

let name_or_label_regex =
   {|([~?`]|let%|and%)?([a-zA-Z0-9_']|[a-zA-Z0-9_'] *\. *)+$|}

let name_or_label_regex_rev =
  Re.compile @@ Re.Posix.re {|^([a-zA-Z0-9_']| *\. *[a-zA-Z0-9_'])+([~?`]|%tel|%dna)?|}

(** matches let%lwt and let* style expressions. See here:https://v2.ocaml.org/manual/bindingops.html *)
let monadic_bind =
  Re.compile @@ Re.Posix.re {|(let|and)([$&*+\-/=>@^|.]|(%[a-zA-Z0-9_']*))$|}
let monadic_bind_rev =
  Re.compile @@ Re.Posix.re {|^([$&*+\-/=>@^|.]|([a-zA-Z0-9_']*)%)(tel|dna)|}

let infixRegex = Re.compile @@ Re.Posix.re {|[~?:!$&*+\-\/=><@^|%<.#]+$|}
let infixRegex_rev = Re.compile @@ Re.Posix.re {|^[~?:!$&*+\-\/=><@^|%<.#]+|}

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
    Re.exec_opt name_or_label_regex_rev text
    |> Option.none_bind (fun () -> Re.exec_opt monadic_bind_rev text)
    |> Option.none_bind (fun () -> Re.exec_opt infixRegex_rev text)
  in
  matched |> Option.map ~f:(fun x -> Group.get x 0)
