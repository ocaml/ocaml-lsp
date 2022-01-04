open Import
open Fiber.O

let folding_range { Range.start; end_ } =
  FoldingRange.create ~startLine:start.line ~startCharacter:start.character
    ~endLine:end_.line ~endCharacter:end_.character ~kind:Region ()

let compute (state : State.t) { FoldingRangeParams.textDocument = { uri }; _ } =
  let ranges = ref [] in
  let push (range : Range.t) =
    if range.end_.line - range.start.line > 1 then ranges := range :: !ranges
  in
  let fold_over_parsetree (parsetree : Mreader.parsetree) =
    let iterator =
      let structure (iterator : Ast_iterator.iterator) structure =
        List.iter structure ~f:(fun structure_item ->
            iterator.structure_item iterator structure_item)
      in
      let signature (iterator : Ast_iterator.iterator) signature =
        List.iter signature ~f:(fun signature_item ->
            iterator.signature_item iterator signature_item)
      in
      let type_declaration (_iterator : Ast_iterator.iterator)
          (type_declaration : Parsetree.type_declaration) =
        let range = Range.of_loc type_declaration.ptype_loc in
        push range
      in
      let module_type_declaration (iterator : Ast_iterator.iterator)
          (module_type_declaration : Parsetree.module_type_declaration) =
        let range = Range.of_loc module_type_declaration.pmtd_loc in
        push range;
        match module_type_declaration.pmtd_type with
        | None -> ()
        | Some module_type -> iterator.module_type iterator module_type
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
      let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression)
          =
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
      let structure_item (iterator : Ast_iterator.iterator)
          (structure_item : Parsetree.structure_item) =
        match structure_item.pstr_desc with
        | Pstr_value (_, value_bindings) ->
          List.iter value_bindings ~f:(fun value_binding ->
              iterator.value_binding iterator value_binding)
        | Pstr_module module_binding ->
          let range = Range.of_loc module_binding.pmb_loc in
          push range;
          let module_expr = module_binding.pmb_expr in
          iterator.module_expr iterator module_expr
        | Pstr_modtype module_type_declaration ->
          iterator.module_type_declaration iterator module_type_declaration
        | Pstr_type (_, type_declarations) ->
          List.iter type_declarations ~f:(fun type_declaration ->
              iterator.type_declaration iterator type_declaration)
        | Pstr_eval (expr, _) -> iterator.expr iterator expr
        | Pstr_primitive _ -> ()
        | Pstr_typext _ -> ()
        | Pstr_exception _ -> ()
        | Pstr_recmodule _ -> ()
        | Pstr_open _ -> ()
        | Pstr_class class_declarations ->
          List.iter class_declarations ~f:(fun class_declaration ->
              iterator.class_declaration iterator class_declaration)
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
      ; module_declaration
      ; module_type
      ; module_type_declaration
      ; signature
      ; signature_item
      ; structure
      ; structure_item
      ; type_declaration
      ; value_binding
      }
    in
    match parsetree with
    | `Interface signature -> iterator.signature iterator signature
    | `Implementation structure -> iterator.structure iterator structure
  in
  let doc = Document_store.get state.store uri in
  let+ () =
    Document.with_pipeline_exn doc (fun pipeline ->
        let parsetree = Mpipeline.reader_parsetree pipeline in
        fold_over_parsetree parsetree)
  in
  Some (List.rev_map ~f:folding_range !ranges)
