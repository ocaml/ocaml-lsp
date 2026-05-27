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

type warning_action =
  | Enable of int
  | Disable of int
  | Enable_as_error of int
  | Enable_range of int * int
  | Disable_range of int * int
  | Enable_as_error_range of int * int
  | Enable_letter of char
  | Disable_letter of char
  | Enable_as_error_letter of char

let parse_warning_payload s =
  let len = String.length s in
  let rec parse i acc =
    if i >= len
    then List.rev acc
    else (
      let sign = s.[i] in
      if sign = '+' || sign = '-' || sign = '@'
      then (
        let j = ref (i + 1) in
        while !j < len && s.[!j] >= '0' && s.[!j] <= '9' do
          incr j
        done;
        let num1_str = String.sub s ~pos:(i + 1) ~len:(!j - i - 1) in
        if num1_str <> ""
        then (
          let num1 = int_of_string num1_str in
          if !j + 1 < len && s.[!j] = '.' && s.[!j + 1] = '.'
          then (
            let k = ref (!j + 2) in
            while !k < len && s.[!k] >= '0' && s.[!k] <= '9' do
              incr k
            done;
            let num2_str = String.sub s ~pos:(!j + 2) ~len:(!k - !j - 2) in
            if num2_str <> ""
            then (
              let num2 = int_of_string num2_str in
              let action =
                match sign with
                | '+' -> Enable_range (num1, num2)
                | '-' -> Disable_range (num1, num2)
                | '@' | _ -> Enable_as_error_range (num1, num2)
              in
              parse !k (action :: acc))
            else parse !k acc)
          else (
            let action =
              match sign with
              | '+' -> Enable num1
              | '-' -> Disable num1
              | '@' | _ -> Enable_as_error num1
            in
            parse !j (action :: acc)))
        else if
          !j < len
          && ((s.[!j] >= 'a' && s.[!j] <= 'z') || (s.[!j] >= 'A' && s.[!j] <= 'Z'))
        then (
          let letter = s.[!j] in
          let action =
            match sign with
            | '+' -> Enable_letter letter
            | '-' -> Disable_letter letter
            | '@' | _ -> Enable_as_error_letter letter
          in
          parse (!j + 1) (action :: acc))
        else parse (!j + 1) acc)
      else parse (i + 1) acc)
  in
  parse 0 []
;;

let format_warning_action action =
  let get_desc n =
    List.find_map
      Ocaml_utils.Warnings.descriptions
      ~f:(fun (description : Ocaml_utils.Warnings.description) ->
        if n = description.number then Some description.description else None)
    |> Option.value ~default:""
  in
  match action with
  | Enable n -> Printf.sprintf "Enables warning %d: %s" n (get_desc n)
  | Disable n -> Printf.sprintf "Disables warning %d: %s" n (get_desc n)
  | Enable_as_error n ->
    Printf.sprintf "Enables warning %d as an error: %s" n (get_desc n)
  | Enable_range (n1, n2) -> Printf.sprintf "Enables warnings %d to %d" n1 n2
  | Disable_range (n1, n2) -> Printf.sprintf "Disables warnings %d to %d" n1 n2
  | Enable_as_error_range (n1, n2) ->
    Printf.sprintf "Enables warnings %d to %d as errors" n1 n2
  | Enable_letter c -> Printf.sprintf "Enables warning set '%c'" c
  | Disable_letter c -> Printf.sprintf "Disables warning set '%c'" c
  | Enable_as_error_letter c -> Printf.sprintf "Enables warning set '%c' as errors" c
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
  let is_on_first_or_last_char ({ loc_start; loc_end; _ } : Ocaml_parsing.Location.t) =
    let start_col = loc_start.pos_cnum - loc_start.pos_bol in
    let end_col = loc_end.pos_cnum - loc_end.pos_bol in
    let at_start = loc_start.pos_lnum = cursor_line && start_col = cursor_col in
    let at_end = loc_end.pos_lnum = cursor_line && end_col = cursor_col + 1 in
    at_start || at_end
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
        let is_on_bracket = is_on_first_or_last_char pattern.ppat_loc in
        if
          is_on_bracket
          || is_at_cursor { pattern.ppat_loc with loc_start = end_of_last_field }
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
      | Pexp_ident { loc; _ }
      | Pexp_construct ({ loc; _ }, _)
      | Pexp_field (_, { loc; _ })
      | Pexp_send (_, { loc; _ })
      | Pexp_new { loc; _ } ->
        if is_at_cursor loc
        then result := Some `Type_enclosing
        else Ast_iterator.default_iterator.expr self expr
      | Pexp_record (fields, _) ->
        (* On a record, each field may be hovered, along with the opening or
        closing brackets. *)
        let is_on_field =
          List.exists fields ~f:(fun (({ loc; _ } : _ Asttypes.loc), _) ->
            is_at_cursor loc)
        in
        let is_on_bracket = is_on_first_or_last_char expr.pexp_loc in
        if is_on_field || is_on_bracket
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
  (* Classes and objects *)
  let class_declaration
        (self : Ast_iterator.iterator)
        (decl : Parsetree.class_declaration)
    =
    if is_at_cursor decl.pci_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.class_declaration self decl
  in
  let class_type_declaration
        (self : Ast_iterator.iterator)
        (decl : Parsetree.class_type_declaration)
    =
    if is_at_cursor decl.pci_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.class_type_declaration self decl
  in
  let class_description
        (self : Ast_iterator.iterator)
        (decl : Parsetree.class_description)
    =
    if is_at_cursor decl.pci_name.loc then result := Some `Type_enclosing;
    Ast_iterator.default_iterator.class_description self decl
  in
  let class_field (self : Ast_iterator.iterator) (field : Parsetree.class_field) =
    (match field.pcf_desc with
     | (Pcf_val ({ loc; _ }, _, _) | Pcf_method ({ loc; _ }, _, _)) when is_at_cursor loc
       -> result := Some `Type_enclosing
     | Pcf_inherit (_, _, Some { loc; _ }) when is_at_cursor loc ->
       result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_field self field
  in
  let class_type_field (self : Ast_iterator.iterator) (field : Parsetree.class_type_field)
    =
    (match field.pctf_desc with
     | (Pctf_val ({ loc; _ }, _, _, _) | Pctf_method ({ loc; _ }, _, _, _))
       when is_at_cursor loc -> result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_type_field self field
  in
  let class_expr (self : Ast_iterator.iterator) (expr : Parsetree.class_expr) =
    (match expr.pcl_desc with
     | Pcl_constr ({ loc; _ }, _) when is_at_cursor loc -> result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_expr self expr
  in
  let class_type (self : Ast_iterator.iterator) (ct : Parsetree.class_type) =
    (match ct.pcty_desc with
     | Pcty_constr ({ loc; _ }, _) when is_at_cursor loc -> result := Some `Type_enclosing
     | _ -> ());
    Ast_iterator.default_iterator.class_type self ct
  in
  (* Hover a module identifier *)
  let module_expr (self : Ast_iterator.iterator) (expr : Parsetree.module_expr) =
    if is_at_cursor expr.pmod_loc
    then (
      match expr.pmod_desc with
      | Pmod_ident { loc; _ } -> if is_at_cursor loc then result := Some `Type_enclosing
      | Pmod_structure _ ->
        let is_at_keyword =
          let keyword_len =
            6
            (* struct *)
          in
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
  let attribute (self : Ast_iterator.iterator) (attr : Parsetree.attribute) =
    if is_at_cursor attr.attr_name.loc || is_at_cursor attr.attr_loc
    then (
      match attr.attr_name.txt with
      | "warning" | "warnerror" ->
        (match attr.attr_payload with
         | PStr
             [ { pstr_desc =
                   Pstr_eval
                     ( { pexp_desc =
                           Pexp_constant
                             { pconst_desc = Pconst_string (payload, _, _); _ }
                       ; _
                       }
                     , _ )
               ; _
               }
             ] ->
           let actions = parse_warning_payload payload in
           let markdown_lines = List.map ~f:format_warning_action actions in
           let markdown = String.concat ~sep:"\n" markdown_lines in
           if markdown <> "" then result := Some (`Warning_attribute markdown)
         | _ -> Ast_iterator.default_iterator.attribute self attr)
      | _ -> Ast_iterator.default_iterator.attribute self attr)
    else Ast_iterator.default_iterator.attribute self attr
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
    ; class_declaration
    ; class_type_declaration
    ; class_description
    ; class_field
    ; class_type_field
    ; class_type
    ; class_expr
    ; attribute
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
      ~warnings_doc
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
         print_dividers (List.filter_opt [ type_info; syntax_doc; doc; warnings_doc ])
       in
       { MarkupContent.value; kind = MarkupKind.Markdown })
     else (
       let value =
         print_dividers (List.filter_opt [ Some typ; syntax_doc; doc; warnings_doc ])
       in
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
    let warnings =
      let active_diagnostics =
        Diagnostics.get_diagnostics (State.diagnostics state) uri
      in
      List.filter_map active_diagnostics ~f:(fun (d : Diagnostic.t) ->
        let start_cmp = Position.compare d.range.start position in
        let end_cmp = Position.compare d.range.end_ position in
        let contains =
          (match start_cmp with
           | Ordering.Lt | Ordering.Eq -> true
           | Ordering.Gt -> false)
          &&
          match end_cmp with
          | Ordering.Gt | Ordering.Eq -> true
          | Ordering.Lt -> false
        in
        if contains
        then (
          match d.code with
          | Some (`Int code) ->
            List.find_map Ocaml_utils.Warnings.descriptions ~f:(fun desc ->
              if desc.number = code then Some (code, desc.description) else None)
          | Some (`String code_str) ->
            List.find_map Ocaml_utils.Warnings.descriptions ~f:(fun desc ->
              if desc.number = int_of_string code_str
              then Some (int_of_string code_str, desc.description)
              else None)
          | None -> None)
        else None)
    in
    let warnings_doc =
      match warnings with
      | [] -> None
      | _ ->
        let text =
          List.map warnings ~f:(fun (code, desc) ->
            sprintf "**Warning %d**: %s" code desc)
          |> String.concat ~sep:"\n\n"
        in
        Some text
    in
    let contents =
      let markdown =
        let client_capabilities = State.client_capabilities state in
        ClientCapabilities.markdown_support client_capabilities ~field:(fun td ->
          Option.map td.hover ~f:(fun h -> h.contentFormat))
      in
      format_type_enclosing
        ~syntax
        ~markdown
        ~typ
        ~doc:documentation
        ~syntax_doc
        ~warnings_doc
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
       | Some (`Warning_attribute markdown) ->
         let contents =
           let client_capabilities = State.client_capabilities state in
           let markdown_support =
             ClientCapabilities.markdown_support client_capabilities ~field:(fun td ->
               Option.map td.hover ~f:(fun h -> h.contentFormat))
           in
           if markdown_support
           then `MarkupContent (MarkupContent.create ~kind:Markdown ~value:markdown)
           else `MarkedString { Lsp.Types.MarkedString.value = markdown; language = None }
         in
         let range = None in
         Fiber.return (Some (Hover.create ~contents ?range ()))
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
