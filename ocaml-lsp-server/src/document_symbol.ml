open Import
open Fiber.O

let core_type_to_string typ =
  ignore (Format.flush_str_formatter ());
  Pprintast.core_type Format.str_formatter typ;
  Format.flush_str_formatter ()
  |> String.map ~f:(function
    | '\n' -> ' '
    | c -> c)
;;

let pattern_to_string pat =
  ignore (Format.flush_str_formatter ());
  Pprintast.pattern Format.str_formatter pat;
  Format.flush_str_formatter ()
;;

let type_document_symbol (decl : Parsetree.type_declaration) : DocumentSymbol.t =
  let kind : SymbolKind.t =
    match decl.ptype_kind with
    | Ptype_variant _ -> Enum
    | _ -> TypeParameter
  in
  let children =
    match decl.ptype_kind with
    | Ptype_variant decls ->
      List.map decls ~f:(fun (decl : Parsetree.constructor_declaration) ->
        DocumentSymbol.create
          ~kind:EnumMember
          ~name:decl.pcd_name.txt
          ~range:(Range.of_loc decl.pcd_loc)
          ~selectionRange:(Range.of_loc decl.pcd_name.loc)
          ())
    | Ptype_record fields ->
      List.map fields ~f:(fun (field : Parsetree.label_declaration) ->
        DocumentSymbol.create
          ~kind:Field
          ~name:field.pld_name.txt
          ~detail:(core_type_to_string field.pld_type)
          ~range:(Range.of_loc field.pld_loc)
          ~selectionRange:(Range.of_loc field.pld_name.loc)
          ())
    | _ -> []
  in
  DocumentSymbol.create
    ~name:decl.ptype_name.txt
    ~kind
    ~range:(Range.of_loc decl.ptype_loc)
    ~selectionRange:(Range.of_loc decl.ptype_loc)
    ~children
    ()
;;

let longident_to_string lident = String.concat ~sep:"." (Longident.flatten lident)

let type_ext_document_symbol (ext : Parsetree.type_extension) : DocumentSymbol.t =
  let children =
    List.map ext.ptyext_constructors ~f:(fun (ext : Parsetree.extension_constructor) ->
      DocumentSymbol.create
        ~name:ext.pext_name.txt
        ~kind:EnumMember
        ~range:(Range.of_loc ext.pext_loc)
        ~selectionRange:(Range.of_loc ext.pext_loc)
        ())
  in
  let range =
    List.fold_left
      children
      ~init:(Range.of_loc ext.ptyext_path.loc)
      ~f:(fun (range : Range.t) (child : DocumentSymbol.t) ->
        let start =
          match Position.compare range.start child.range.start with
          | Lt | Eq -> range.start
          | Gt -> child.range.start
        in
        let end_ =
          match Position.compare range.end_ child.range.end_ with
          | Lt | Eq -> child.range.end_
          | Gt -> range.end_
        in
        Range.create ~start ~end_)
  in
  DocumentSymbol.create
    ~name:(longident_to_string ext.ptyext_path.txt)
    ~kind:Enum
    ~range
    ~selectionRange:(Range.of_loc ext.ptyext_path.loc)
    ~children
    ()
;;

let value_document_symbol (value : Parsetree.value_description) =
  let kind : SymbolKind.t =
    match value.pval_type.ptyp_desc with
    | Ptyp_arrow _ -> Function
    | _ -> Variable
  in
  DocumentSymbol.create
    ~kind
    ~name:value.pval_name.txt
    ~detail:(core_type_to_string value.pval_type)
    ~range:(Range.of_loc value.pval_loc)
    ~selectionRange:(Range.of_loc value.pval_name.loc)
    ()
;;

let module_decl_document_symbol (pmod : Parsetree.module_declaration) ~children =
  DocumentSymbol.create
    ~name:(Option.value pmod.pmd_name.txt ~default:"_")
    ~kind:Module
    ~range:(Range.of_loc pmod.pmd_loc)
    ~selectionRange:(Range.of_loc pmod.pmd_name.loc)
    ~children
    ()
;;

let module_type_decl_symbol (decl : Parsetree.module_type_declaration) ~children =
  DocumentSymbol.create
    ~name:decl.pmtd_name.txt
    ~kind:Interface
    ~range:(Range.of_loc decl.pmtd_loc)
    ~selectionRange:(Range.of_loc decl.pmtd_name.loc)
    ~children
    ()
;;

let module_binding_document_symbol (pmod : Parsetree.module_binding) ~children =
  DocumentSymbol.create
    ~name:(Option.value pmod.pmb_name.txt ~default:"_")
    ~kind:Module
    ~range:(Range.of_loc pmod.pmb_loc)
    ~selectionRange:(Range.of_loc pmod.pmb_name.loc)
    ~children
    ()
;;

let binding_document_symbol
  (binding : Parsetree.value_binding)
  ~ppx
  ~is_top_level
  ~children
  =
  let variables_in_pattern (pattern : Parsetree.pattern) =
    let symbols = ref [] in
    let pat (iterator : Ast_iterator.iterator) (pattern : Parsetree.pattern) =
      match pattern.ppat_desc with
      | Ppat_var name ->
        let symbol =
          DocumentSymbol.create
            ~kind:Variable
            ~name:name.txt
            ~range:(Range.of_loc name.loc)
            ~selectionRange:(Range.of_loc name.loc)
            ()
        in
        symbols := symbol :: !symbols
      | _ -> Ast_iterator.default_iterator.pat iterator pattern
    in
    let iterator = { Ast_iterator.default_iterator with pat } in
    iterator.pat iterator pattern;
    List.rev !symbols
  in
  let name =
    match binding.pvb_pat.ppat_desc with
    | Ppat_var name | Ppat_extension (_, PPat ({ ppat_desc = Ppat_var name; _ }, _)) ->
      `Parent name.txt
    | _ ->
      (match is_top_level, children with
       | true, [] | false, _ -> `Variables (variables_in_pattern binding.pvb_pat)
       | true, _ :: _ ->
         (match ppx with
          | Some ppx -> `Parent (ppx ^ ": " ^ pattern_to_string binding.pvb_pat)
          | None -> `Parent (pattern_to_string binding.pvb_pat)))
  in
  match name with
  | `Parent name ->
    let kind : SymbolKind.t =
      match ppx, binding.pvb_expr.pexp_desc with
      | None, (Pexp_function _ | Pexp_newtype _) -> Function
      | Some _, _ -> Property
      | _ -> Variable
    in
    let detail =
      None
      (* CR-rgrinberg: Re-enable in 5.0: {[
           Option.map binding.pvb_constraint ~f:(function
               | Pvc_constraint { typ; _ } -> core_type_to_string typ
               | Pvc_coercion { coercion; _ } -> core_type_to_string coercion)
         ]}
      *)
    in
    [ DocumentSymbol.create
        ~name
        ~kind
        ?detail
        ~range:(Range.of_loc binding.pvb_loc)
        ~selectionRange:(Range.of_loc binding.pvb_pat.ppat_loc)
        ~children
        ()
    ]
  | `Variables symbols -> symbols @ children
;;

let symbols_from_parsetree parsetree =
  let current = ref [] in
  let descend
    (iter : unit -> unit)
    (get_current_symbol : children:DocumentSymbol.t list -> DocumentSymbol.t)
    =
    let outer = !current in
    current := [];
    iter ();
    current := outer @ [ get_current_symbol ~children:!current ]
  in
  let signature_item (iterator : Ast_iterator.iterator) (item : Parsetree.signature_item) =
    match item.psig_desc with
    | Psig_type (_, decls) -> current := !current @ List.map decls ~f:type_document_symbol
    | Psig_typext ext -> current := !current @ [ type_ext_document_symbol ext ]
    | Psig_value value -> current := !current @ [ value_document_symbol value ]
    | Psig_module pmd ->
      descend
        (fun () -> Ast_iterator.default_iterator.signature_item iterator item)
        (module_decl_document_symbol pmd)
    | Psig_recmodule modules ->
      List.iter modules ~f:(iterator.module_declaration iterator)
    | Psig_modtype decl ->
      descend
        (fun () -> Ast_iterator.default_iterator.module_type_declaration iterator decl)
        (module_type_decl_symbol decl)
    | _ -> Ast_iterator.default_iterator.signature_item iterator item
  in
  let rec structure_item
    ~ppx
    (iterator : Ast_iterator.iterator)
    (item : Parsetree.structure_item)
    =
    match item.pstr_desc with
    | Pstr_type (_, decls) -> current := !current @ List.map decls ~f:type_document_symbol
    | Pstr_typext ext -> current := !current @ [ type_ext_document_symbol ext ]
    | Pstr_module pmod ->
      descend
        (fun () -> iterator.module_expr iterator pmod.pmb_expr)
        (module_binding_document_symbol pmod)
    | Pstr_recmodule modules -> List.iter modules ~f:(iterator.module_binding iterator)
    | Pstr_modtype decl ->
      descend
        (fun () -> Ast_iterator.default_iterator.module_type_declaration iterator decl)
        (module_type_decl_symbol decl)
    | Pstr_value (_, bindings) ->
      let outer = !current in
      current
      := outer
         @ List.concat_map bindings ~f:(fun (binding : Parsetree.value_binding) ->
           current := [];
           iterator.expr iterator binding.pvb_expr;
           binding_document_symbol binding ~ppx ~is_top_level:true ~children:!current)
    | Pstr_extension ((name, PStr items), _) ->
      List.iter items ~f:(fun item -> structure_item ~ppx:(Some name.txt) iterator item)
    | _ -> Ast_iterator.default_iterator.structure_item iterator item
  in
  let expr (iterator : Ast_iterator.iterator) (item : Parsetree.expression) =
    match item.pexp_desc with
    | Pexp_let (_, bindings, inner) ->
      let outer = !current in
      let bindings =
        List.concat_map bindings ~f:(fun (binding : Parsetree.value_binding) ->
          current := [];
          iterator.expr iterator binding.pvb_expr;
          binding_document_symbol binding ~ppx:None ~is_top_level:false ~children:!current)
      in
      current := outer @ bindings;
      iterator.expr iterator inner
    | _ -> Ast_iterator.default_iterator.expr iterator item
  in
  let iterator =
    { Ast_iterator.default_iterator with
      signature_item
    ; structure_item = structure_item ~ppx:None
    ; expr
    }
  in
  let () =
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  !current
;;

let rec flatten_document_symbols ~uri ~container_name (symbols : DocumentSymbol.t list) =
  List.concat_map symbols ~f:(fun symbol ->
    let symbol_information =
      SymbolInformation.create
        ?containerName:container_name
        ~kind:symbol.kind
        ~location:{ range = symbol.range; uri }
        ~name:symbol.name
        ()
    in
    let children =
      flatten_document_symbols
        ~uri
        ~container_name:(Some symbol.name)
        (Option.value symbol.children ~default:[])
    in
    symbol_information :: children)
;;

let run (client_capabilities : ClientCapabilities.t) doc uri =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin _ ->
    let+ symbols =
      Document.Merlin.with_pipeline_exn
        ~name:"document-symbols"
        (Document.merlin_exn doc)
        (fun pipeline -> Mpipeline.reader_parsetree pipeline |> symbols_from_parsetree)
    in
    (match
       Option.value
         ~default:false
         (let open Option.O in
          let* textDocument = client_capabilities.textDocument in
          let* ds = textDocument.documentSymbol in
          ds.hierarchicalDocumentSymbolSupport)
     with
     | true -> Some (`DocumentSymbol symbols)
     | false ->
       let flattened = flatten_document_symbols ~uri ~container_name:None symbols in
       Some (`SymbolInformation flattened))
;;
