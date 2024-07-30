open Import
open Fiber.O

type mode =
  | Default
  | Extended_fixed of int
  | Extended_variable

(* possibly overwrite the default mode using an environment variable *)
let environment_mode =
  match Env_vars._IS_HOVER_EXTENDED () with
  | Some true -> Extended_variable
  | Some false | None -> Default
;;

let hover_at_cursor parsetree (`Logical (cursor_line, cursor_col)) =
  let result = ref None in
  let is_at_cursor ({ loc_start; loc_end; _ } : Ocaml_parsing.Location.t) =
    let start_col = loc_start.pos_cnum - loc_start.pos_bol in
    let end_col = loc_end.pos_cnum - loc_end.pos_bol in
    let at_or_after_start =
      loc_start.pos_lnum < cursor_line
      || (loc_start.pos_lnum = cursor_line && start_col <= cursor_col)
    in
    let before_or_at_end =
      loc_end.pos_lnum > cursor_line
      || (loc_end.pos_lnum = cursor_line && end_col >= cursor_col)
    in
    at_or_after_start && before_or_at_end
  in
  (* Hover location matches a variable binding *)
  let pat (self : Ast_iterator.iterator) (pattern : Parsetree.pattern) =
    if is_at_cursor pattern.ppat_loc
    then (
      match pattern.ppat_desc with
      | Ppat_any | Ppat_constant _ | Ppat_variant _ | Ppat_unpack _ ->
        result := Some `Type_enclosing
      | Ppat_record (fields, Open) ->
        let end_of_last_field =
          match List.last fields with
          | Some (_, field) -> field.ppat_loc.loc_end
          | None -> pattern.ppat_loc.loc_start
        in
        if is_at_cursor { pattern.ppat_loc with loc_start = end_of_last_field }
        then result := Some `Type_enclosing
      | Ppat_construct ({ loc; _ }, _)
      | Ppat_var { loc; _ }
      | Ppat_alias (_, { loc; _ })
      | Ppat_type { loc; _ }
      | Ppat_open ({ loc; _ }, _) ->
        if is_at_cursor loc then result := Some `Type_enclosing
      | _ -> ());
    Ast_iterator.default_iterator.pat self pattern
  in
  (* Hover an identifier in an expression *)
  let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
    if is_at_cursor expr.pexp_loc
    then (
      match expr.pexp_desc with
      | Pexp_constant _ | Pexp_variant _ | Pexp_pack _ -> result := Some `Type_enclosing
      | Pexp_ident { loc; _ } | Pexp_construct ({ loc; _ }, _) | Pexp_field (_, { loc; _ })
        ->
        if is_at_cursor loc
        then result := Some `Type_enclosing
        else Ast_iterator.default_iterator.expr self expr
      | Pexp_record (fields, _) ->
        (* On a record, each field may be hovered. *)
        let is_on_field =
          List.exists fields ~f:(fun (({ loc; _ } : _ Asttypes.loc), _) ->
            is_at_cursor loc)
        in
        if is_on_field
        then result := Some `Type_enclosing
        else Ast_iterator.default_iterator.expr self expr
      | Pexp_function _ | Pexp_lazy _ ->
        (* Anonymous function expressions can be hovered on the keyword [fun] or
           [function]. Lazy expressions can also be hovered on the [lazy]
           keyword. *)
        let is_at_keyword =
          let keyword_len =
            match expr.pexp_desc with
            | Pexp_function _ -> 8
            | Pexp_lazy _ -> 4
            | _ -> 0
          in
          let pos_cnum = expr.pexp_loc.loc_start.pos_cnum + keyword_len in
          let end_of_keyword = { expr.pexp_loc.loc_start with pos_cnum } in
          is_at_cursor
            { loc_start = expr.pexp_loc.loc_start
            ; loc_end = end_of_keyword
            ; loc_ghost = false
            }
        in
        if is_at_keyword
        then result := Some `Type_enclosing
        else Ast_iterator.default_iterator.expr self expr
      | Pexp_extension (ppx, _) when is_at_cursor ppx.loc ->
        result := Some (`Ppx_expr (expr, ppx))
      | _ -> Ast_iterator.default_iterator.expr self expr)
  in
  (* Hover a value declaration in a signature *)
  let value_description
    (self : Ast_iterator.iterator)
    (desc : Parsetree.value_description)
    =
    if is_at_cursor desc.pval_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.value_description self desc
  in
  (* Hover a type *)
  let typ (_ : Ast_iterator.iterator) (typ : Parsetree.core_type) =
    if is_at_cursor typ.ptyp_loc then result := Some `Type_enclosing
  in
  (* Hover a type declaration *)
  let type_declaration (self : Ast_iterator.iterator) (decl : Parsetree.type_declaration) =
    if is_at_cursor decl.ptype_name.loc
    then result := Some `Type_enclosing
    else if is_at_cursor decl.ptype_loc
    then (
      let attribute_at_cursor =
        List.find decl.ptype_attributes ~f:(fun attr -> is_at_cursor attr.attr_loc)
      in
      match attribute_at_cursor with
      | Some attr ->
        (* Produce a hover for the attribute, if it's name is hovered, otherwise
           bail. *)
        if is_at_cursor attr.attr_name.loc
        then result := Some (`Ppx_typedef_attr (decl, attr))
      | None -> Ast_iterator.default_iterator.type_declaration self decl)
  in
  (* Hover a module identifier *)
  let module_expr (self : Ast_iterator.iterator) (expr : Parsetree.module_expr) =
    if is_at_cursor expr.pmod_loc
    then (
      match expr.pmod_desc with
      | Pmod_ident { loc; _ } -> if is_at_cursor loc then result := Some `Type_enclosing
      | Pmod_structure _ ->
        let is_at_keyword =
          let keyword_len = 6 (* struct *) in
          let pos_cnum = expr.pmod_loc.loc_start.pos_cnum + keyword_len in
          is_at_cursor
            { loc_start = expr.pmod_loc.loc_start
            ; loc_end = { expr.pmod_loc.loc_start with pos_cnum }
            ; loc_ghost = false
            }
        in
        if is_at_keyword then result := Some `Type_enclosing
      | _ -> ());
    Ast_iterator.default_iterator.module_expr self expr
  in
  (* Hover a module type *)
  let module_type (self : Ast_iterator.iterator) (mtyp : Parsetree.module_type) =
    if is_at_cursor mtyp.pmty_loc
    then (
      match mtyp.pmty_desc with
      | Pmty_ident { loc; _ } -> if is_at_cursor loc then result := Some `Type_enclosing
      | _ -> ());
    Ast_iterator.default_iterator.module_type self mtyp
  in
  (* Hover structure items *)
  let structure_item (self : Ast_iterator.iterator) (item : Parsetree.structure_item) =
    match item.pstr_desc with
    | Pstr_module desc when is_at_cursor desc.pmb_name.loc ->
      result := Some `Type_enclosing
    | _ -> Ast_iterator.default_iterator.structure_item self item
  in
  (* Hover signature items *)
  let signature_item (self : Ast_iterator.iterator) (item : Parsetree.signature_item) =
    match item.psig_desc with
    | Psig_open desc when is_at_cursor desc.popen_expr.loc ->
      (* [open X] is not captured by [module_expr] since it uses a different
         type in the AST. *)
      result := Some `Type_enclosing
    | Psig_module desc when is_at_cursor desc.pmd_name.loc ->
      result := Some `Type_enclosing
    | _ -> Ast_iterator.default_iterator.signature_item self item
  in
  let iterator =
    { Ast_iterator.default_iterator with
      pat
    ; expr
    ; typ
    ; type_declaration
    ; value_description
    ; module_expr
    ; module_type
    ; structure_item
    ; signature_item
    }
  in
  let () =
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  !result
;;

let print_dividers sections = String.concat ~sep:"\n***\n" sections

let format_as_code_block ~highlighter strings =
  sprintf "```%s\n%s\n```" highlighter (String.concat ~sep:" " strings)
;;

let format_type_enclosing
  ~syntax
  ~markdown
  ~typ
  ~doc
  ~(syntax_doc : Query_protocol.syntax_doc_result option)
  =
  (* TODO for vscode, we should just use the language id. But that will not work
     for all editors *)
  let syntax_doc =
    Option.map syntax_doc ~f:(fun syntax_doc ->
      sprintf
        "`syntax` %s: %s. See [Manual](%s)"
        syntax_doc.name
        syntax_doc.description
        syntax_doc.documentation)
  in
  `MarkupContent
    (if markdown
     then (
       let value =
         let markdown_name = Document.Syntax.markdown_name syntax in
         let type_info = Some (format_as_code_block ~highlighter:markdown_name [ typ ]) in
         let doc =
           Option.map doc ~f:(fun doc ->
             match Doc_to_md.translate doc with
             | Raw d -> d
             | Markdown d -> d)
         in
         print_dividers (List.filter_opt [ type_info; syntax_doc; doc ])
       in
       { MarkupContent.value; kind = MarkupKind.Markdown })
     else (
       let value = print_dividers (List.filter_opt [ Some typ; syntax_doc; doc ]) in
       { MarkupContent.value; kind = MarkupKind.PlainText }))
;;

let format_ppx_expansion ~ppx ~expansion =
  let value = sprintf "(* ppx %s expansion *)\n%s" ppx expansion in
  `MarkedString { Lsp.Types.MarkedString.value; language = Some "ocaml" }
;;

let type_enclosing_hover
  ~(server : State.t Server.t)
  ~(doc : Document.t)
  ~with_syntax_doc
  ~merlin
  ~mode
  ~uri
  ~position
  =
  let state = Server.state server in
  let verbosity =
    let mode =
      match mode, environment_mode with
      | Default, Extended_variable -> Extended_variable
      | x, _ -> x
    in
    match mode with
    | Default -> 0
    | Extended_fixed v ->
      state.hover_extended.history <- None;
      v
    | Extended_variable ->
      let v =
        match state.hover_extended.history with
        | None -> 0
        | Some (h_uri, h_position, h_verbosity) ->
          if Uri.equal uri h_uri && Ordering.is_eq (Position.compare position h_position)
          then succ h_verbosity
          else 0
      in
      state.hover_extended.history <- Some (uri, position, v);
      v
  in
  let* type_enclosing =
    Document.Merlin.type_enclosing
      ~name:"hover-enclosing"
      merlin
      (Position.logical position)
      verbosity
      ~with_syntax_doc
  in
  match type_enclosing with
  | None -> Fiber.return None
  | Some { Document.Merlin.loc; typ; doc = documentation; syntax_doc } ->
    let syntax = Document.syntax doc in
    let* typ =
      (* We ask Ocamlformat to format this type *)
      let* result = Ocamlformat_rpc.format_type state.ocamlformat_rpc ~typ in
      match result with
      | Ok v ->
        (* OCamlformat adds an unnecessay newline at the end of the type *)
        Fiber.return (String.trim v)
      | Error `No_process -> Fiber.return typ
      | Error (`Msg message) ->
        (* We log OCamlformat errors and display the unformated type *)
        let+ () =
          let message =
            sprintf
              "An error occured while querying ocamlformat:\nInput type: %s\n\nAnswer: %s"
              typ
              message
          in
          State.log_msg server ~type_:Warning ~message
        in
        typ
    in
    let contents =
      let markdown =
        let client_capabilities = State.client_capabilities state in
        ClientCapabilities.markdown_support client_capabilities ~field:(fun td ->
          Option.map td.hover ~f:(fun h -> h.contentFormat))
      in
      format_type_enclosing ~syntax ~markdown ~typ ~doc:documentation ~syntax_doc
    in
    let range = Range.of_loc loc in
    let hover = Hover.create ~contents ~range () in
    Fiber.return (Some hover)
;;

let ppx_expression_hover
  ~ppx_parsetree
  ~(expr : Parsetree.expression)
  ~(ppx : string Asttypes.loc)
  =
  let expanded_ppx = ref None in
  let at_expr_location (loc : Ocaml_parsing.Location.t) =
    expr.pexp_loc.loc_start.pos_cnum <= loc.loc_start.pos_cnum
    && loc.loc_end.pos_cnum <= expr.pexp_loc.loc_end.pos_cnum
  in
  (* Locate the first expression at the original PPX location. *)
  let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
    match at_expr_location expr.pexp_loc with
    | true -> expanded_ppx := Some expr
    | false -> Ast_iterator.default_iterator.expr self expr
  in
  let iterator = { Ast_iterator.default_iterator with expr } in
  let () =
    match ppx_parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  Option.map !expanded_ppx ~f:(fun expr ->
    let range =
      Range.of_loc
        (if ppx.loc.loc_start.pos_cnum < expr.pexp_loc.loc_start.pos_cnum
         then { ppx.loc with loc_end = expr.pexp_loc.loc_end }
         else expr.pexp_loc)
    in
    let contents =
      format_ppx_expansion
        ~ppx:ppx.txt
        ~expansion:(Ocaml_parsing.Pprintast.string_of_expression expr)
    in
    Hover.create ~contents ~range ())
;;

let typedef_attribute_hover
  ~ppx_parsetree
  ~(decl : Parsetree.type_declaration)
  ~(attr : Parsetree.attribute)
  =
  match attr.attr_name.txt with
  | "deriving" ->
    let signature = ref [] in
    let structure = ref [] in
    let at_decl_location (loc : Ocaml_parsing.Location.t) =
      decl.ptype_loc.loc_start.pos_cnum <= loc.loc_start.pos_cnum
      && decl.ptype_loc.loc_end.pos_cnum >= loc.loc_start.pos_cnum
    in
    let contains_decl (decls : Parsetree.type_declaration list) =
      List.exists decls ~f:(fun (decl' : Parsetree.type_declaration) ->
        String.equal decl.ptype_name.txt decl'.ptype_name.txt)
    in
    let signature_item (self : Ast_iterator.iterator) (item : Parsetree.signature_item) =
      match at_decl_location item.psig_loc with
      | true ->
        (match item.psig_desc with
         | Psig_type (_, decls) when contains_decl decls ->
           (* Don't add the type declaration itself. *) ()
         | _ -> signature := item :: !signature)
      | false -> Ast_iterator.default_iterator.signature_item self item
    in
    let structure_item (self : Ast_iterator.iterator) (item : Parsetree.structure_item) =
      match at_decl_location item.pstr_loc with
      | true ->
        (match item.pstr_desc with
         | Pstr_type (_, decls) when contains_decl decls ->
           (* Don't add the type definition itself. *) ()
         | _ -> structure := item :: !structure)
      | false -> Ast_iterator.default_iterator.structure_item self item
    in
    let iterator =
      { Ast_iterator.default_iterator with signature_item; structure_item }
    in
    let () =
      match ppx_parsetree with
      | `Interface signature -> iterator.signature iterator signature
      | `Implementation structure -> iterator.structure iterator structure
    in
    let expansion =
      match !signature, !structure with
      | [], [] -> None
      | signature, [] ->
        ignore (Format.flush_str_formatter ());
        Pprintast.signature Format.str_formatter (List.rev signature);
        Some (Format.flush_str_formatter ())
      | [], structure -> Some (Pprintast.string_of_structure (List.rev structure))
      | _ :: _, _ :: _ ->
        (* This should not be possible, unless a PPXs provides incorrect
           position information that places items from a [sig end] into a
           [struct end] or vice versa. *)
        None
    in
    Option.map expansion ~f:(fun expansion ->
      let range = Range.of_loc attr.attr_loc in
      let contents = format_ppx_expansion ~ppx:attr.attr_name.txt ~expansion in
      Hover.create ~contents ~range ())
  | _ -> None
;;

let handle server { HoverParams.textDocument = { uri }; position; _ } mode =
  Fiber.of_thunk (fun () ->
    let state : State.t = Server.state server in
    let doc =
      let store = state.store in
      Document_store.get store uri
    in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin merlin ->
      let* parsetree =
        Document.Merlin.with_pipeline_exn
          ~name:"hover"
          (Document.merlin_exn doc)
          (fun pipeline -> Mpipeline.reader_parsetree pipeline)
      in
      (match hover_at_cursor parsetree (Position.logical position) with
       | None -> Fiber.return None
       | Some `Type_enclosing ->
         let with_syntax_doc =
           match state.configuration.data.syntax_documentation with
           | Some { enable = true } -> true
           | Some _ | None -> false
         in
         type_enclosing_hover ~server ~doc ~merlin ~mode ~uri ~position ~with_syntax_doc
       | Some ((`Ppx_expr _ | `Ppx_typedef_attr _) as ppx_kind) ->
         let+ ppx_parsetree =
           Document.Merlin.with_pipeline_exn
             ~name:"expand-ppx"
             (Document.merlin_exn doc)
             (fun pipeline -> Mpipeline.ppx_parsetree pipeline)
         in
         (match ppx_kind with
          | `Ppx_expr (expr, ppx) -> ppx_expression_hover ~ppx_parsetree ~expr ~ppx
          | `Ppx_typedef_attr (decl, attr) ->
            typedef_attribute_hover ~ppx_parsetree ~decl ~attr)))
;;
