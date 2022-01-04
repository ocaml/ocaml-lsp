open Import
open Fiber.O

let folding_range { Range.start; end_ } =
  FoldingRange.create ~startLine:start.line ~startCharacter:start.character
    ~endLine:end_.line ~endCharacter:end_.character ~kind:Region ()

let fold_over_parsetree (parsetree : Mreader.parsetree) =
  let ranges = ref [] in
  let push (range : Range.t) =
    if range.start.line < range.end_.line (* don't fold a single line *) then
      ranges := range :: !ranges
  in
  let iterator =
    let type_declaration (_self : Ast_iterator.iterator)
        (typ_decl : Parsetree.type_declaration) =
      Range.of_loc typ_decl.ptype_loc |> push
    in
    let module_type_declaration (self : Ast_iterator.iterator)
        (mod_typ_decl : Parsetree.module_type_declaration) =
      Range.of_loc mod_typ_decl.pmtd_loc |> push;
      Option.iter mod_typ_decl.pmtd_type ~f:(fun mod_typ ->
          self.module_type self mod_typ)
    in
    let module_type (iterator : Ast_iterator.iterator)
        (module_type : Parsetree.module_type) =
      match module_type.pmty_desc with
      | Pmty_ident _ -> ()
      | Pmty_signature signature -> iterator.signature iterator signature
      | Pmty_functor (_, _) -> ()
      | Pmty_with (_, _) -> ()
      | Pmty_typeof _ -> ()
      | Pmty_extension _ -> ()
      | Pmty_alias _ -> ()
    in
    let module_declaration (iterator : Ast_iterator.iterator)
        (module_declaration : Parsetree.module_declaration) =
      let range = Range.of_loc module_declaration.pmd_loc in
      push range;
      iterator.module_type iterator module_declaration.pmd_type
    in
    let class_declaration (_iterator : Ast_iterator.iterator)
        (class_declaration : Parsetree.class_declaration) =
      let range = Range.of_loc class_declaration.pci_loc in
      push range
    in
    let value_binding (iterator : Ast_iterator.iterator)
        (value_binding : Parsetree.value_binding) =
      let range = Range.of_loc value_binding.pvb_loc in
      push range;
      iterator.expr iterator value_binding.pvb_expr
    in
    let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
      match expr.pexp_desc with
      | Pexp_let (_, value_bindings, expr) ->
        List.iter value_bindings ~f:(fun value_binding ->
            iterator.value_binding iterator value_binding);
        iterator.expr iterator expr
      | Pexp_fun (_, _, _, expr) -> iterator.expr iterator expr
      | Pexp_open (_, expr) -> iterator.expr iterator expr
      | Pexp_letop letop ->
        (* Location is not correct. It include the location of the whole
           expression. See: https://github.com/ocaml/ocaml/pull/10682 *)
        let range = Range.of_loc letop.let_.pbop_loc in
        push range;
        iterator.expr iterator letop.let_.pbop_exp
      | Pexp_extension (_, payload) -> (
        match payload with
        | PStr structure -> iterator.structure iterator structure
        | PSig signature -> iterator.signature iterator signature
        | PTyp _ -> ()
        | PPat (_, _) -> ())
      | Pexp_ident _ -> ()
      | Pexp_constant _ -> ()
      | Pexp_function _ -> ()
      | Pexp_apply (_, _) -> ()
      | Pexp_match (_, _) -> ()
      | Pexp_try (_, _) -> ()
      | Pexp_tuple _ -> ()
      | Pexp_construct (_, _) -> ()
      | Pexp_variant (_, _) -> ()
      | Pexp_record (_, _) -> ()
      | Pexp_field (_, _) -> ()
      | Pexp_setfield (_, _, _) -> ()
      | Pexp_array _ -> ()
      | Pexp_ifthenelse (_, _, _) -> ()
      | Pexp_sequence (_, _) -> ()
      | Pexp_while (_, _) -> ()
      | Pexp_for (_, _, _, _, _) -> ()
      | Pexp_constraint (_, _) -> ()
      | Pexp_coerce (_, _, _) -> ()
      | Pexp_send (_, _) -> ()
      | Pexp_new _ -> ()
      | Pexp_setinstvar (_, _) -> ()
      | Pexp_override _ -> ()
      | Pexp_letmodule (_, _, _) -> ()
      | Pexp_letexception (_, _) -> ()
      | Pexp_assert _ -> ()
      | Pexp_lazy _ -> ()
      | Pexp_poly (_, _) -> ()
      | Pexp_object _ -> ()
      | Pexp_newtype (_, _) -> ()
      | Pexp_pack _ -> ()
      | Pexp_unreachable -> ()
      | Pexp_hole -> ()
    in
    let module_binding (self : Ast_iterator.iterator)
        (module_binding : Parsetree.module_binding) =
      let range = Range.of_loc module_binding.pmb_loc in
      push range;
      let module_expr = module_binding.pmb_expr in
      self.module_expr self module_expr
    in
    let open_declaration (self : Ast_iterator.iterator)
        (open_decl : Parsetree.open_declaration) =
      let open_decl_range = open_decl.Parsetree.popen_loc |> Range.of_loc in
      push open_decl_range;
      self.module_expr self open_decl.popen_expr
    in
    let structure_item self structure_item =
      match structure_item.Parsetree.pstr_desc with
      | Pstr_value (_, _)
      | Pstr_class _
      | Pstr_modtype _
      | Pstr_type (_, _)
      | Pstr_module _
      | Pstr_eval (_, _)
      | Pstr_recmodule _
      | Pstr_open _ ->
        Ast_iterator.default_iterator.structure_item self structure_item
      | Pstr_primitive _ -> ()
      | Pstr_typext _ -> ()
      | Pstr_exception _ -> ()
      | Pstr_class_type _ -> ()
      | Pstr_include _ -> ()
      | Pstr_attribute _ -> ()
      | Pstr_extension (_, _) -> ()
    in
    let signature_item (iterator : Ast_iterator.iterator)
        (signature_item : Parsetree.signature_item) =
      match signature_item.psig_desc with
      | Psig_value _ -> ()
      | Psig_type (_, type_declarations) ->
        List.iter type_declarations ~f:(fun type_declaration ->
            iterator.type_declaration iterator type_declaration)
      | Psig_typesubst _ -> ()
      | Psig_typext _ -> ()
      | Psig_exception _ -> ()
      | Psig_module module_declaration ->
        iterator.module_declaration iterator module_declaration
      | Psig_modsubst _ -> ()
      | Psig_recmodule _ -> ()
      | Psig_modtype module_type_declaration ->
        iterator.module_type_declaration iterator module_type_declaration
      | Psig_modtypesubst _ -> ()
      | Psig_open _ -> ()
      | Psig_include _ -> ()
      | Psig_class _ -> ()
      | Psig_class_type _ -> ()
      | Psig_attribute _ -> ()
      | Psig_extension (_, _) -> ()
    in
    { Ast_iterator.default_iterator with
      class_declaration
    ; expr
    ; module_binding
    ; module_declaration
    ; module_type
    ; module_type_declaration
    ; open_declaration
    ; signature_item
    ; structure_item
    ; type_declaration
    ; value_binding
    }
  in
  let () =
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  List.rev_map !ranges ~f:folding_range

let compute (state : State.t) (params : FoldingRangeParams.t) =
  let doc = Document_store.get state.store params.textDocument.uri in
  let+ ranges =
    Document.with_pipeline_exn doc (fun pipeline ->
        let parsetree = Mpipeline.reader_parsetree pipeline in
        fold_over_parsetree parsetree)
  in
  Some ranges
