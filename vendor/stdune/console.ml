module Display = struct
  type t =
    | Progress
    | Short
    | Verbose
    | Quiet

  let all =
    [ ("progress", Progress)
    ; ("verbose", Verbose)
    ; ("short", Short)
    ; ("quiet", Quiet)
    ]
end

let reset_terminal () =
  print_string "\x1bc";
  flush stdout

module T = struct
  type t =
    { display : Display.t
    ; mutable status_line : Ansi_color.Style.t list Pp.t
    ; mutable status_line_len : int
    }

  let hide_status_line t =
    if t.status_line_len > 0 then Printf.eprintf "\r%*s\r" t.status_line_len ""

  let show_status_line t =
    if t.status_line_len > 0 then Ansi_color.prerr t.status_line

  let update_status_line t status_line =
    if t.display = Progress then (
      let status_line =
        Pp.map_tags status_line ~f:User_message.Print_config.default
      in
      let status_line_len =
        String.length (Format.asprintf "%a" Pp.render_ignore_tags status_line)
      in
      hide_status_line t;
      t.status_line <- status_line;
      t.status_line_len <- status_line_len;
      show_status_line t;
      flush stderr
    )

  let print t msg =
    hide_status_line t;
    prerr_string msg;
    show_status_line t;
    flush stderr

  let print_user_message t ?config msg =
    hide_status_line t;
    Option.iter msg.User_message.loc ~f:(Loc.print Format.err_formatter);
    User_message.prerr ?config { msg with loc = None };
    show_status_line t;
    flush stderr

  let clear_status_line t =
    hide_status_line t;
    t.status_line <- Pp.nop;
    t.status_line_len <- 0;
    flush stderr
end

let t_var = ref None

let init display =
  t_var := Some { T.display; status_line = Pp.nop; status_line_len = 0 }

let t () = Option.value_exn !t_var

let display () = (t ()).display

module Status_line = struct
  type t = unit -> User_message.Style.t Pp.t option

  let status_line = ref (Fn.const None)

  let refresh () =
    match !status_line () with
    | None -> T.clear_status_line (t ())
    | Some pp ->
      (* Always put the status line inside a horizontal to force the [Format]
         module to prefer a single line. In particular, it seems that
         [Format.pp_print_text] split sthe line before the last word, unless it
         is succeeded by a space. This seems like a bug in [Format] and putting
         the whole thing into a [hbox] works around this bug.

         See https://github.com/ocaml/dune/issues/2779 *)
      T.update_status_line (t ()) (Pp.hbox pp)

  let set x =
    status_line := x;
    refresh ()

  let set_temporarily x f =
    let old = !status_line in
    set x;
    Exn.protect ~finally:(fun () -> set old) ~f
end

let print msg =
  match !t_var with
  | None -> Printf.eprintf "%s%!" msg
  | Some t -> T.print t msg

let print_user_message ?config msg =
  match !t_var with
  | None -> User_message.prerr ?config msg
  | Some t -> T.print_user_message t ?config msg

let () = User_warning.set_reporter print_user_message
