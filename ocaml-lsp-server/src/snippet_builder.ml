open Import
module Lexer = Ocaml_preprocess.Lexer_raw
module Parser = Ocaml_preprocess.Parser_raw

type t =
  { snippet : Snippet.t
  ; placeholders : int
  }

let rec lexer_result = function
  | Lexer.Return token -> token
  | Refill refill -> lexer_result (refill ())
  | Fail _ -> raise Parser.Error
;;

let lexer_token lexer lexbuf = lexer_result (Lexer.token_without_comments lexer lexbuf)

let expression_hole_ranges source =
  match
    let lexbuf = Lexing.from_string source in
    let lexer = Lexer.make (Lexer.keywords []) in
    Parser.parse_expression (lexer_token lexer) lexbuf
  with
  | exception Parser.Error -> Error ()
  | expression ->
    let ranges = ref [] in
    let expr iterator (expression : Parsetree.expression) =
      (match expression.pexp_desc with
       | Pexp_extension ({ txt; loc }, PStr [])
         when String.equal txt Ocaml_parsing.Ast_helper.hole_txt ->
         (* The identifier location covers just the hole, whereas [pexp_loc]
            can include parentheses and attributes. Check the source to exclude
            explicit [%merlin.hole] extensions. *)
         let start = loc.loc_start.pos_cnum in
         let stop = loc.loc_end.pos_cnum in
         if
           start >= 0
           && stop = start + 1
           && stop <= String.length source
           && Char.equal source.[start] '_'
         then ranges := (start, stop) :: !ranges
       | _ -> ());
      Ocaml_parsing.Ast_iterator.default_iterator.expr iterator expression
    in
    let iterator = { Ocaml_parsing.Ast_iterator.default_iterator with expr } in
    iterator.expr iterator expression;
    Ok
      (List.dedup_and_sort !ranges ~compare:(fun (start, _) (start', _) ->
         Int.compare start start'))
;;

let source ~source =
  let ranges =
    match expression_hole_ranges source with
    | Ok ranges -> ranges
    | Error () -> []
  in
  let rec snippets offset = function
    | [] -> [ Snippet.text (String.drop_prefix source offset); Snippet.tabstop 0 ]
    | (start, stop) :: ranges ->
      Snippet.text (String.sub source ~pos:offset ~len:(start - offset))
      :: Snippet.placeholder (Snippet.text "_")
      :: snippets stop ranges
  in
  { snippet = Snippet.concat (snippets 0 ranges); placeholders = List.length ranges }
;;

let argument_labels typ =
  match
    let lexbuf = Lexing.from_string typ in
    let lexer = Lexer.make (Lexer.keywords []) in
    Parser.parse_core_type (lexer_token lexer) lexbuf
  with
  | exception Parser.Error -> None
  | typ ->
    let rec loop labels (typ : Parsetree.core_type) =
      match typ.ptyp_desc with
      | Ptyp_arrow (label, _argument, result) -> loop (label :: labels) result
      | Ptyp_alias (typ, _) | Ptyp_poly (_, typ) -> loop labels typ
      | _ -> List.rev labels
    in
    Some (loop [] typ)
;;

let valid_application_name name =
  String.split name ~on:'.'
  |> List.for_all ~f:(fun segment ->
    (not (String.is_empty segment))
    && (match segment.[0] with
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
        | _ -> false)
    && String.for_all segment ~f:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false))
;;

let application ~name ~typ =
  match valid_application_name name with
  | false -> None
  | true ->
    let open Option.O in
    let* labels = argument_labels typ in
    if
      List.count labels ~f:(function
        | Asttypes.Nolabel -> false
        | Labelled _ | Optional _ -> true)
      < 2
    then None
    else (
      let arguments =
        let placeholder = [ Snippet.placeholder (Snippet.text "_") ] in
        List.concat_map labels ~f:(fun (label : Asttypes.arg_label) ->
          Snippet.text
            (match label with
             | Nolabel -> " "
             | Labelled label -> " ~" ^ label ^ ":"
             | Optional label -> " ?" ^ label ^ ":")
          :: placeholder)
      in
      Some (Snippet.concat ((Snippet.text name :: arguments) @ [ Snippet.tabstop 0 ])))
;;
