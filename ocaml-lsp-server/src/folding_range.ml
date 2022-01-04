open Import
open Fiber.O

let folding_range { Range.start; end_ } =
  FoldingRange.create ~startLine:start.line ~startCharacter:start.character
    ~endLine:end_.line ~endCharacter:end_.character ~kind:Region ()

let compute (state : State.t) { FoldingRangeParams.textDocument = { uri }; _ } =
  let ranges = ref [] in
  let push (range : Range.t) =
    if range.end_.line - range.start.line >= 2 then
      ranges := folding_range range :: !ranges
    else
      ()
  in
  let fold_over_parsetree (parsetree : Mreader.parsetree) =
    let iterator =
      let default_iterator = Ast_iterator.default_iterator in
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
        | Parsetree.Pmty_ident _ -> ()
        | Parsetree.Pmty_signature signature ->
          iterator.signature iterator signature
        | Parsetree.Pmty_functor (_, _) -> ()
        | Parsetree.Pmty_with (_, _) -> ()
        | Parsetree.Pmty_typeof _ -> ()
        | Parsetree.Pmty_extension _ -> ()
        | Parsetree.Pmty_alias _ -> ()
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
        | Parsetree.Pexp_let (_, value_bindings, expr) ->
          List.iter value_bindings ~f:(fun value_binding ->
              iterator.value_binding iterator value_binding);
          iterator.expr iterator expr
        | Parsetree.Pexp_fun (_, _, _, expr) -> iterator.expr iterator expr
        | Parsetree.Pexp_open (_, expr) -> iterator.expr iterator expr
        | Parsetree.Pexp_letop letop ->
          (* Location is not correct. It include the location of the whole
             expression. See: https://github.com/ocaml/ocaml/pull/10682 *)
          let range = Range.of_loc letop.let_.pbop_loc in
          push range;
          iterator.expr iterator letop.let_.pbop_exp
        | Parsetree.Pexp_extension (_, payload) -> (
          match payload with
          | Parsetree.PStr structure -> iterator.structure iterator structure
          | Parsetree.PSig signature -> iterator.signature iterator signature
          | Parsetree.PTyp _ -> ()
          | Parsetree.PPat (_, _) -> ())
        | Parsetree.Pexp_ident _ -> ()
        | Parsetree.Pexp_constant _ -> ()
        | Parsetree.Pexp_function _ -> ()
        | Parsetree.Pexp_apply (_, _) -> ()
        | Parsetree.Pexp_match (_, _) -> ()
        | Parsetree.Pexp_try (_, _) -> ()
        | Parsetree.Pexp_tuple _ -> ()
        | Parsetree.Pexp_construct (_, _) -> ()
        | Parsetree.Pexp_variant (_, _) -> ()
        | Parsetree.Pexp_record (_, _) -> ()
        | Parsetree.Pexp_field (_, _) -> ()
        | Parsetree.Pexp_setfield (_, _, _) -> ()
        | Parsetree.Pexp_array _ -> ()
        | Parsetree.Pexp_ifthenelse (_, _, _) -> ()
        | Parsetree.Pexp_sequence (_, _) -> ()
        | Parsetree.Pexp_while (_, _) -> ()
        | Parsetree.Pexp_for (_, _, _, _, _) -> ()
        | Parsetree.Pexp_constraint (_, _) -> ()
        | Parsetree.Pexp_coerce (_, _, _) -> ()
        | Parsetree.Pexp_send (_, _) -> ()
        | Parsetree.Pexp_new _ -> ()
        | Parsetree.Pexp_setinstvar (_, _) -> ()
        | Parsetree.Pexp_override _ -> ()
        | Parsetree.Pexp_letmodule (_, _, _) -> ()
        | Parsetree.Pexp_letexception (_, _) -> ()
        | Parsetree.Pexp_assert _ -> ()
        | Parsetree.Pexp_lazy _ -> ()
        | Parsetree.Pexp_poly (_, _) -> ()
        | Parsetree.Pexp_object _ -> ()
        | Parsetree.Pexp_newtype (_, _) -> ()
        | Parsetree.Pexp_pack _ -> ()
        | Parsetree.Pexp_unreachable -> ()
        | Parsetree.Pexp_hole -> ()
      in
      let structure_item (iterator : Ast_iterator.iterator)
          (structure_item : Parsetree.structure_item) =
        match structure_item.pstr_desc with
        | Parsetree.Pstr_value (_, value_bindings) ->
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
        | Parsetree.Pstr_eval (expr, _) -> iterator.expr iterator expr
        | Parsetree.Pstr_primitive _ -> ()
        | Parsetree.Pstr_typext _ -> ()
        | Parsetree.Pstr_exception _ -> ()
        | Parsetree.Pstr_recmodule _ -> ()
        | Parsetree.Pstr_open _ -> ()
        | Parsetree.Pstr_class class_declarations ->
          List.iter class_declarations ~f:(fun class_declaration ->
              iterator.class_declaration iterator class_declaration)
        | Parsetree.Pstr_class_type _ -> ()
        | Parsetree.Pstr_include _ -> ()
        | Parsetree.Pstr_attribute _ -> ()
        | Parsetree.Pstr_extension (_, _) -> ()
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
      { default_iterator with
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
  Some (List.rev !ranges)
