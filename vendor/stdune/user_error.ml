exception E of User_message.t

let prefix =
  Pp.seq
    (Pp.tag (Pp.verbatim "Error") ~tag:User_message.Style.Error)
    (Pp.char ':')

let make ?loc ?hints paragraphs =
  User_message.make ?loc ?hints paragraphs ~prefix

let raise ?loc ?hints paragraphs = raise (E (make ?loc ?hints paragraphs))

let () =
  Printexc.register_printer (function
    | E t ->
      Some (Format.asprintf "%a@?" Pp.render_ignore_tags (User_message.pp t))
    | _ -> None)
