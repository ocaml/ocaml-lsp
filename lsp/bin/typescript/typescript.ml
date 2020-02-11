open Import

type test =
  { snippet : string
  ; loc : Lexing.position
  ; exn : exn
  }

let test_snippets s =
  List.filter_map s ~f:(fun s ->
      let lexbuf = Lexing.from_string s in
      try
        ignore (Ts_parser.main Ts_lexer.token lexbuf);
        None
      with exn -> Some { snippet = s; loc = lexbuf.lex_curr_p; exn })

let pp_results ppf tests =
  List.iteri tests ~f:(fun i { snippet; loc; exn } ->
      Format.pp_print_string ppf snippet;
      Format.fprintf ppf "line: %d char: %d@." loc.pos_lnum
        (loc.pos_cnum - loc.pos_bol);
      Format.fprintf ppf "%d. exn: %s@.%!" (i + 1) (Printexc.to_string exn);
      Format.fprintf ppf "@.---@.")

let of_snippets s =
  List.concat_map s ~f:(fun s ->
      let lexbuf = Lexing.from_string s in
      try Ts_parser.main Ts_lexer.token lexbuf with _exn -> [])
