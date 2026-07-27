open Import
module Lexer = Ocaml_preprocess.Lexer_raw
module Parser = Ocaml_preprocess.Parser_raw

type t =
  { snippet : Snippet.t
  ; placeholders : int
  }

type hole_kind =
  [ `Expression
  | `After_arrow
  ]

let rec lexer_result = function
  | Lexer.Return token -> token
  | Refill refill -> lexer_result (refill ())
  | Fail _ -> raise Parser.Error
;;

let lexer_token lexer lexbuf = lexer_result (Lexer.token_without_comments lexer lexbuf)

let expression_hole_ranges ~context ~holes source =
  let arrow_holes = ref (Set.empty (module Int)) in
  match
    let next_source_token =
      let lexer = Lexer.make (Lexer.keywords []) in
      match holes with
      | `Expression -> lexer_token lexer
      | `After_arrow ->
        let previous = ref Parser.EOF in
        fun lexbuf ->
          let token = lexer_token lexer lexbuf in
          (match token, !previous with
           | Parser.UNDERSCORE, Parser.MINUSGREATER ->
             arrow_holes := Set.add !arrow_holes (Lexing.lexeme_start lexbuf)
           | _ -> ());
          previous := token;
          token
    in
    let prefix, suffix =
      match context with
      | `Expression -> [], []
      | `Cases -> [ Parser.FUNCTION ], []
      | `Cases_with_final_pattern ->
        [ Parser.FUNCTION ], [ Parser.MINUSGREATER; Parser.LPAREN; Parser.RPAREN ]
    in
    let prefix = Queue.of_list prefix in
    let suffix = Queue.of_list suffix in
    (* Synthetic tokens have zero-width locations at the start or EOF. Lexing
       the original source keeps every real token's location unchanged. *)
    let next lexbuf =
      match Queue.dequeue prefix with
      | Some token -> token
      | None ->
        (match next_source_token lexbuf with
         | Parser.EOF -> Option.value (Queue.dequeue suffix) ~default:Parser.EOF
         | token -> token)
    in
    Parser.parse_expression next (Lexing.from_string source)
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
    let ranges =
      List.dedup_and_sort !ranges ~compare:(fun (start, _) (start', _) ->
        Int.compare start start')
    in
    Ok
      (match holes with
       | `Expression -> ranges
       | `After_arrow ->
         (* An underscore after an arrow can also be a type wildcard. Retain only
            candidates confirmed to be expression holes by the parser. *)
         List.filter ranges ~f:(fun (start, _) -> Set.mem !arrow_holes start))
;;

let source ~holes ~source =
  let ranges =
    let contexts =
      match holes with
      | `Expression -> [ `Expression ]
      | `After_arrow -> [ `Expression; `Cases; `Cases_with_final_pattern ]
    in
    List.find_map contexts ~f:(fun context ->
      match expression_hole_ranges ~context ~holes source with
      | Error () -> None
      | Ok ranges -> Some ranges)
    |> Option.value ~default:[]
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

let valid_application_name ~kind name =
  let lexbuf = Lexing.from_string name in
  let lexer = Lexer.make (Lexer.keywords []) in
  let rec path () =
    match lexer_token lexer lexbuf with
    | Parser.UIDENT _ ->
      (match lexer_token lexer lexbuf with
       | Parser.DOT -> path ()
       | EOF -> kind = `Constructor
       | _ -> false)
    | Parser.LIDENT _ -> kind = `Function && lexer_token lexer lexbuf = Parser.EOF
    | _ -> false
  in
  match path () with
  | result -> result
  | exception Parser.Error -> false
;;

let constructor_arity typ =
  match
    let lexbuf = Lexing.from_string typ in
    let lexer = Lexer.make (Lexer.keywords []) in
    let rec tokens () =
      match lexer_token lexer lexbuf with
      | Parser.EOF -> [ Parser.EOF ]
      | token -> token :: tokens ()
    in
    let rec normalize = function
      (* Merlin renders an inline-record payload as [t.Constructor], which is not
         an OCaml type path. Use an opaque type while preserving its arity. *)
      | Parser.LIDENT _ :: Parser.DOT :: Parser.UIDENT _ :: rest ->
        Parser.UIDENT "Inline_record" :: Parser.DOT :: Parser.LIDENT "t" :: normalize rest
      | token :: rest -> token :: normalize rest
      | [] -> []
    in
    (* Parse a constructor signature, not a function type: parentheses distinguish
       [int * bool -> t] from [(int * bool) -> t]. *)
    let tokens =
      Queue.of_list
        ([ Parser.TYPE; Parser.LIDENT "t"; Parser.EQUAL; Parser.UIDENT "C"; Parser.COLON ]
         @ normalize (tokens ()))
    in
    Parser.implementation
      (fun _ -> Option.value (Queue.dequeue tokens) ~default:Parser.EOF)
      lexbuf
  with
  | [ { pstr_desc =
          Pstr_type (_, [ { ptype_kind = Ptype_variant [ { pcd_args; _ } ]; _ } ])
      ; _
      }
    ] ->
    Some
      (match pcd_args with
       | Pcstr_tuple arguments -> List.length arguments
       | Pcstr_record _ -> 1)
  | _ -> None
  | exception Parser.Error -> None
;;

type application_kind =
  [ `Function
  | `Constructor
  ]

let application ~kind ~name ~typ =
  match valid_application_name ~kind name with
  | false -> None
  | true ->
    let open Option.O in
    let placeholder = Snippet.placeholder (Snippet.text "_") in
    let* arguments =
      match kind with
      | `Constructor ->
        let* arity = constructor_arity typ in
        if arity = 0
        then None
        else (
          let payload =
            if arity = 1
            then placeholder
            else
              Snippet.concat
                [ Snippet.text "("
                ; Snippet.concat
                    (List.init arity ~f:(fun _ -> placeholder)
                     |> List.intersperse ~sep:(Snippet.text ", "))
                ; Snippet.text ")"
                ]
          in
          Some [ Snippet.text " "; payload ])
      | `Function ->
        let* labels = argument_labels typ in
        if
          List.count labels ~f:(function
            | Asttypes.Nolabel -> false
            | Labelled _ | Optional _ -> true)
          < 2
        then None
        else
          Some
            (List.concat_map labels ~f:(fun (label : Asttypes.arg_label) ->
               [ Snippet.text
                   (match label with
                    | Nolabel -> " "
                    | Labelled label -> " ~" ^ label ^ ":"
                    | Optional label -> " ?" ^ label ^ ":")
               ; placeholder
               ]))
    in
    let prefix, suffix =
      match kind with
      | `Function -> name, ""
      | `Constructor -> "(" ^ name, ")"
    in
    Some
      (Snippet.concat
         ((Snippet.text prefix :: arguments) @ [ Snippet.text suffix; Snippet.tabstop 0 ]))
;;
