open Import
open Option.O
module H = Ocaml_parsing.Ast_helper

let range_contains_loc range loc =
  match Range.of_loc_opt loc with
  | Some range' -> Range.contains range range'
  | None -> false
;;

let range_contained_by_loc range loc =
  match Range.of_loc_opt loc with
  | Some range' -> Range.contains range' range
  | None -> false
;;

let largest_enclosed_expression typedtree range =
  let exception Found of Typedtree.expression in
  let module I = Ocaml_typing.Tast_iterator in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    if range_contains_loc range expr.exp_loc
    then raise (Found expr)
    else I.default_iterator.expr iter expr
  in
  let iterator = { I.default_iterator with expr = expr_iter } in
  try
    iterator.structure iterator typedtree;
    None
  with
  | Found e -> Some e
;;

let enclosing_structure_item typedtree range =
  let exception Found of Typedtree.structure_item in
  let module I = Ocaml_typing.Tast_iterator in
  let structure_item_iter (iter : I.iterator) (item : Typedtree.structure_item) =
    if range_contained_by_loc range item.str_loc
    then (
      match item.str_desc with
      | Tstr_value _ -> raise (Found item)
      | _ -> I.default_iterator.structure_item iter item)
  in
  let iterator = { I.default_iterator with structure_item = structure_item_iter } in
  try
    iterator.structure iterator typedtree;
    None
  with
  | Found e -> Some e
;;

let tightest_enclosing_binder_position typedtree range =
  let exception Found of Position.t in
  let module I = Ocaml_typing.Tast_iterator in
  let found_loc loc =
    Position.of_lexical_position loc |> Option.iter ~f:(fun p -> raise (Found p))
  in
  let found_if_expr_contains (expr : Typedtree.expression) =
    let loc = expr.exp_loc in
    if range_contained_by_loc range loc then found_loc loc.loc_start
  in
  let found_if_case_contains cases =
    List.iter cases ~f:(fun (case : _ Typedtree.case) ->
      found_if_expr_contains case.c_rhs)
  in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    if range_contained_by_loc range expr.exp_loc
    then (
      I.default_iterator.expr iter expr;
      match expr.exp_desc with
      | Texp_let (_, _, body)
      | Texp_while (_, body)
      | Texp_for (_, _, _, _, _, body)
      | Texp_letmodule (_, _, _, _, body)
      | Texp_letexception (_, body)
      | Texp_open (_, body) -> found_if_expr_contains body
      | Texp_letop { body; _ } -> found_if_case_contains [ body ]
      | Texp_function (_, Tfunction_cases { cases; _ }) -> found_if_case_contains cases
      | Texp_match (_, cases, _) -> found_if_case_contains cases
      | Texp_try (_, cases) -> found_if_case_contains cases
      | _ -> ())
  in
  let structure_item_iter (iter : I.iterator) (item : Typedtree.structure_item) =
    if range_contained_by_loc range item.str_loc
    then (
      I.default_iterator.structure_item iter item;
      match item.str_desc with
      | Tstr_value (_, bindings) ->
        List.iter bindings ~f:(fun (binding : Typedtree.value_binding) ->
          found_if_expr_contains binding.vb_expr)
      | _ -> ())
  in
  let iterator =
    { I.default_iterator with expr = expr_iter; structure_item = structure_item_iter }
  in
  try
    iterator.structure iterator typedtree;
    None
  with
  | Found e -> Some e
;;

module LongidentSet = Set.Make (struct
    type t = Longident.t

    let compare = compare
  end)

(** [free expr] returns the free variables in [expr]. *)
let free (expr : Typedtree.expression) =
  let module I = Ocaml_typing.Tast_iterator in
  let idents = ref [] in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (path, { txt = ident; _ }, _) -> idents := (ident, path) :: !idents
    | _ ->
      I.default_iterator.expr iter expr;
      (* if a variable was bound but is no longer, it must be associated with a
         binder inside the expression *)
      idents
      := List.filter !idents ~f:(fun (ident, path) ->
           match Env.find_value_by_name ident expr.exp_env with
           | path', _ -> Path.same path path'
           | exception Not_found -> false)
  in
  let iter = { I.default_iterator with expr = expr_iter } in
  iter.expr iter expr;
  !idents
;;

let must_pass expr env =
  List.filter (free expr) ~f:(fun (ident, path) ->
    match Env.find_value_by_name ident env with
    | path', _ ->
      (* new environment binds ident to a different path than the old one *)
      not (Path.same path path')
    | exception Not_found -> true)
  |> List.map ~f:fst
;;

let extract_local doc typedtree range =
  let* to_extract = largest_enclosed_expression typedtree range in
  let* extract_range = Range.of_loc_opt to_extract.exp_loc in
  let* edit_pos = tightest_enclosing_binder_position typedtree range in
  let new_name = "var_name" in
  let* local_text = Document.substring doc extract_range in
  let newText = sprintf "let %s = %s in\n" new_name local_text in
  let insert_range = { Range.start = edit_pos; end_ = edit_pos } in
  Some
    [ TextEdit.create ~newText ~range:insert_range
    ; TextEdit.create ~newText:new_name ~range:extract_range
    ]
;;

let extract_function doc typedtree range =
  let* to_extract = largest_enclosed_expression typedtree range in
  let* extract_range = Range.of_loc_opt to_extract.exp_loc in
  let* parent_item = enclosing_structure_item typedtree range in
  let* edit_pos = Position.of_lexical_position parent_item.str_loc.loc_start in
  let new_name = "fun_name" in
  let* args_str =
    let free_vars = must_pass to_extract parent_item.str_env in
    let+ args =
      List.map free_vars ~f:(function
        | Longident.Lident id -> Some id
        | _ -> None)
      |> Option.List.all
    in
    let s = String.concat ~sep:" " args in
    if String.is_empty s then "()" else s
  in
  let* func_text = Document.substring doc extract_range in
  let new_function = sprintf "let %s %s = %s\n\n" new_name args_str func_text in
  let new_call = sprintf "%s %s" new_name args_str in
  let insert_range = { Range.start = edit_pos; end_ = edit_pos } in
  Some
    [ TextEdit.create ~newText:new_function ~range:insert_range
    ; TextEdit.create ~newText:new_call ~range:extract_range
    ]
;;

let run_extract_local pipeline doc (params : CodeActionParams.t) =
  let typer = Mpipeline.typer_result pipeline in
  let* typedtree =
    match Mtyper.get_typedtree typer with
    | `Interface _ -> None
    | `Implementation x -> Some x
  in
  let+ edits = extract_local doc typedtree params.range in
  CodeAction.create
    ~title:"Extract local"
    ~kind:CodeActionKind.RefactorExtract
    ~edit:(Document.edit doc edits)
    ~isPreferred:false
    ()
;;

let run_extract_function pipeline doc (params : CodeActionParams.t) =
  let typer = Mpipeline.typer_result pipeline in
  let* typedtree =
    match Mtyper.get_typedtree typer with
    | `Interface _ -> None
    | `Implementation x -> Some x
  in
  let+ edits = extract_function doc typedtree params.range in
  CodeAction.create
    ~title:"Extract function"
    ~kind:CodeActionKind.RefactorExtract
    ~edit:(Document.edit doc edits)
    ~isPreferred:false
    ()
;;

let local = Code_action.batchable RefactorExtract run_extract_local
let function_ = Code_action.batchable RefactorExtract run_extract_function
