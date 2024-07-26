open Import
open Fiber.O

let folding_range { Range.start; end_ } =
  FoldingRange.create
    ~startLine:start.line
    ~startCharacter:start.character
    ~endLine:end_.line
    ~endCharacter:end_.character
    ~kind:Region
    ()
;;

let fold_over_parsetree (parsetree : Mreader.parsetree) =
  let ranges = ref [] in
  let push (range : Range.t) =
    if range.start.line < range.end_.line (* don't fold a single line *)
    then ranges := range :: !ranges
  in
  let iterator =
    let type_declaration
      (_self : Ast_iterator.iterator)
      (typ_decl : Parsetree.type_declaration)
      =
      Range.of_loc typ_decl.ptype_loc |> push
    in
    let type_extension
      (_self : Ast_iterator.iterator)
      (typ_ext : Parsetree.type_extension)
      =
      let loc = typ_ext.ptyext_path.loc in
      let last_constr = List.last typ_ext.ptyext_constructors in
      let loc =
        match last_constr with
        | None -> loc
        | Some { pext_loc; _ } -> { loc with loc_end = pext_loc.loc_end }
      in
      Range.of_loc loc |> push
    in
    let module_type_declaration
      (self : Ast_iterator.iterator)
      (mod_typ_decl : Parsetree.module_type_declaration)
      =
      Range.of_loc mod_typ_decl.pmtd_loc |> push;
      Option.iter mod_typ_decl.pmtd_type ~f:(fun mod_typ -> self.module_type self mod_typ)
    in
    let module_type (self : Ast_iterator.iterator) (module_type : Parsetree.module_type) =
      match module_type.pmty_desc with
      | Pmty_signature _ | Pmty_functor _ ->
        let range = Range.of_loc module_type.pmty_loc in
        push range;
        Ast_iterator.default_iterator.module_type self module_type
      | Pmty_ident _ | Pmty_with _ | Pmty_typeof _ | Pmty_extension _ | Pmty_alias _ ->
        Ast_iterator.default_iterator.module_type self module_type
    in
    let module_declaration
      (self : Ast_iterator.iterator)
      (module_declaration : Parsetree.module_declaration)
      =
      let range = Range.of_loc module_declaration.pmd_loc in
      push range;
      self.module_type self module_declaration.pmd_type
    in
    let module_expr (self : Ast_iterator.iterator) (module_expr : Parsetree.module_expr) =
      match module_expr.pmod_desc with
      | Parsetree.Pmod_functor (_, _) | Parsetree.Pmod_structure _ ->
        let range = Range.of_loc module_expr.pmod_loc in
        push range;
        Ast_iterator.default_iterator.module_expr self module_expr
      | _ ->
        (* We rely on the wildcard pattern to improve compatibility with
           multiple OCaml's parsetree versions *)
        Ast_iterator.default_iterator.module_expr self module_expr
    in
    let class_declaration
      (self : Ast_iterator.iterator)
      (class_decl : Parsetree.class_declaration)
      =
      class_decl.Parsetree.pci_loc |> Range.of_loc |> push;
      self.class_expr self class_decl.pci_expr
    in
    let class_description
      (self : Ast_iterator.iterator)
      (class_desc : Parsetree.class_description)
      =
      class_desc.pci_loc |> Range.of_loc |> push;
      self.class_type self class_desc.pci_expr
    in
    let class_expr (self : Ast_iterator.iterator) (class_expr : Parsetree.class_expr) =
      class_expr.pcl_loc |> Range.of_loc |> push;
      Ast_iterator.default_iterator.class_expr self class_expr
    in
    let class_field (self : Ast_iterator.iterator) (class_field : Parsetree.class_field) =
      Range.of_loc class_field.pcf_loc |> push;
      Ast_iterator.default_iterator.class_field self class_field
    in
    let class_type (self : Ast_iterator.iterator) (class_type : Parsetree.class_type) =
      Range.of_loc class_type.pcty_loc |> push;
      Ast_iterator.default_iterator.class_type self class_type
    in
    let class_type_declaration
      (self : Ast_iterator.iterator)
      (class_type_decl : Parsetree.class_type_declaration)
      =
      Range.of_loc class_type_decl.pci_loc |> push;
      Ast_iterator.default_iterator.class_type_declaration self class_type_decl
    in
    let class_type_field
      (self : Ast_iterator.iterator)
      (class_type_field : Parsetree.class_type_field)
      =
      Range.of_loc class_type_field.pctf_loc |> push;
      Ast_iterator.default_iterator.class_type_field self class_type_field
    in
    let value_binding
      (self : Ast_iterator.iterator)
      (value_binding : Parsetree.value_binding)
      =
      let range = Range.of_loc value_binding.pvb_loc in
      push range;
      self.expr self value_binding.pvb_expr
    in
    let extension (self : Ast_iterator.iterator) ((_, payload) : Parsetree.extension) =
      match payload with
      | PStr structure -> self.structure self structure
      | PSig signature -> self.signature self signature
      | PTyp _ -> ()
      | PPat (_, _) -> ()
    in
    let case (self : Ast_iterator.iterator) (c : Parsetree.case) =
      let pat_range = Range.of_loc c.pc_lhs.ppat_loc in
      push pat_range;
      let expr_range = Range.of_loc c.pc_rhs.pexp_loc in
      push { Range.start = pat_range.end_; end_ = expr_range.end_ };
      self.expr self c.pc_rhs
    in
    let pat (self : Ast_iterator.iterator) (p : Parsetree.pattern) =
      let open Parsetree in
      match p.ppat_desc with
      | Ppat_record (bdgs, _) ->
        Range.of_loc p.ppat_loc |> push;
        List.iter bdgs ~f:(fun (lident, pat) ->
          let lident_range = Range.of_loc lident.Asttypes.loc in
          let pat_range = Range.of_loc pat.Parsetree.ppat_loc in
          push { Range.start = lident_range.end_; end_ = pat_range.end_ })
      | Ppat_var _
      | Ppat_alias _
      | Ppat_constant _
      | Ppat_interval _
      | Ppat_tuple _
      | Ppat_construct _
      | Ppat_variant _
      | Ppat_array _
      | Ppat_or _
      | Ppat_constraint _
      | Ppat_type _
      | Ppat_lazy _
      | Ppat_unpack _
      | Ppat_exception _
      | Ppat_extension _
      | Ppat_open _
      | Ppat_any -> Ast_iterator.default_iterator.pat self p
    in
    let expr (self : Ast_iterator.iterator) (expr : Parsetree.expression) =
      match expr.pexp_desc with
      | Pexp_try (e, cases) | Pexp_match (e, cases) ->
        Range.of_loc expr.pexp_loc |> push;
        self.expr self e;
        self.cases self cases
      | Pexp_letop letop ->
        (* Location is not correct. It include the location of the whole
           expression. See: https://github.com/ocaml/ocaml/pull/10682 *)
        let range = Range.of_loc letop.let_.pbop_loc in
        push range;
        Ast_iterator.default_iterator.expr self expr
      | Pexp_record (bdgs, old_record) ->
        Range.of_loc expr.pexp_loc |> push;
        Option.iter old_record ~f:(self.expr self);
        List.iter bdgs ~f:(fun (lident, expr) ->
          let lident_range = Range.of_loc lident.Asttypes.loc in
          let expr_range = Range.of_loc expr.Parsetree.pexp_loc in
          push { Range.start = lident_range.end_; end_ = expr_range.end_ })
      | Pexp_ifthenelse (if_expr, then_expr, else_expr) ->
        let loc = { if_expr.pexp_loc with loc_end = then_expr.pexp_loc.loc_end } in
        Range.of_loc loc |> push;
        self.expr self then_expr;
        Option.iter else_expr ~f:(self.expr self)
      | Pexp_apply _
      | Pexp_while _
      | Pexp_for _
      | Pexp_object _
      | Pexp_pack _
      | Pexp_letmodule _ ->
        Range.of_loc expr.pexp_loc |> push;
        Ast_iterator.default_iterator.expr self expr
      | Pexp_extension _
      | Pexp_let _
      | Pexp_open _
      | Pexp_poly _
      | Pexp_sequence _
      | Pexp_constraint _
      | Pexp_function _
      | Pexp_newtype _
      | Pexp_lazy _
      | Pexp_letexception _
      | Pexp_tuple _
      | Pexp_construct _
      | Pexp_ident _
      | Pexp_constant _
      | Pexp_variant _
      | Pexp_field _
      | Pexp_setfield _
      | Pexp_array _
      | Pexp_coerce _
      | Pexp_send _
      | Pexp_new _
      | Pexp_setinstvar _
      | Pexp_override _
      | Pexp_assert _
      | Pexp_unreachable -> Ast_iterator.default_iterator.expr self expr
    in
    let module_binding
      (self : Ast_iterator.iterator)
      (module_binding : Parsetree.module_binding)
      =
      Range.of_loc module_binding.pmb_loc |> push;
      self.module_expr self module_binding.pmb_expr
    in
    let open_declaration
      (self : Ast_iterator.iterator)
      (open_decl : Parsetree.open_declaration)
      =
      Range.of_loc open_decl.popen_loc |> push;
      self.module_expr self open_decl.popen_expr
    in
    let value_description
      (_self : Ast_iterator.iterator)
      (value_desc : Parsetree.value_description)
      =
      Range.of_loc value_desc.pval_loc |> push
    in
    let structure_item self structure_item =
      match structure_item.Parsetree.pstr_desc with
      | Pstr_value _
      | Pstr_class _
      | Pstr_modtype _
      | Pstr_type _
      | Pstr_module _
      | Pstr_eval _
      | Pstr_recmodule _
      | Pstr_class_type _
      | Pstr_typext _
      | Pstr_open _ -> Ast_iterator.default_iterator.structure_item self structure_item
      | Pstr_extension _ ->
        Range.of_loc structure_item.pstr_loc |> push;
        Ast_iterator.default_iterator.structure_item self structure_item
      | Pstr_include { pincl_loc; pincl_mod; pincl_attributes } ->
        push @@ Range.of_loc pincl_loc;
        self.module_expr self pincl_mod;
        self.attributes self pincl_attributes
      | Pstr_primitive _ | Pstr_exception _ | Pstr_attribute _ -> ()
    in
    { Ast_iterator.default_iterator with
      case
    ; class_declaration
    ; class_description
    ; class_expr
    ; class_field
    ; class_type
    ; class_type_declaration
    ; class_type_field
    ; expr
    ; extension
    ; module_binding
    ; module_declaration
    ; module_expr
    ; module_type
    ; module_type_declaration
    ; open_declaration
    ; pat
    ; structure_item
    ; type_declaration
    ; type_extension
    ; value_binding
    ; value_description
    }
  in
  let () =
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  List.rev_map !ranges ~f:folding_range
;;

let compute (state : State.t) (params : FoldingRangeParams.t) =
  Fiber.of_thunk (fun () ->
    let doc = Document_store.get state.store params.textDocument.uri in
    match Document.kind doc with
    | `Other -> Fiber.return None
    | `Merlin m ->
      let+ ranges =
        Document.Merlin.with_pipeline_exn ~name:"folding range" m (fun pipeline ->
          let parsetree = Mpipeline.reader_parsetree pipeline in
          fold_over_parsetree parsetree)
      in
      Some ranges)
;;
