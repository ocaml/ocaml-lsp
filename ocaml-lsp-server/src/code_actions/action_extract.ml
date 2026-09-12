open Import
open Option.O
module H = Ocaml_parsing.Ast_helper
module Typedtree_utils = Merlin_analysis.Typedtree_utils

type extraction =
  { range : Range.t
  ; insert_range : Range.t
  ; name : string
  ; binding_suffix : string
  ; call_suffix : string
  }

let workspace_edit state doc { range; insert_range; name; binding_suffix; call_suffix } =
  let text_document = Document.text_document doc in
  if State.client_capabilities state |> Capabilities.workspace_edit_snippet_support
  then
    (* Only snippet clients need to copy the source between the insertion and
       replacement. Keep the ordinary-edit fallback independent of this work. *)
    let+ before_expression =
      Text_document.substring
        text_document
        (Range.create ~start:insert_range.start ~end_:range.start)
    in
    let name = Snippet.placeholder ~index:1 (Snippet.text name) in
    let snippet =
      Snippet.concat
        [ Snippet.text "let "
        ; name
        ; Snippet.text binding_suffix
        ; Snippet.text before_expression
        ; name
        ; Snippet.text call_suffix
        ; Snippet.tabstop 0
        ]
    in
    let edit =
      SnippetTextEdit.create
        ~range:(Range.create ~start:insert_range.start ~end_:range.end_)
        ~snippet:(StringValue.create ~value:(Snippet.to_string snippet))
        ()
    in
    Text_document.workspace_edit_of_edits text_document [ `SnippetTextEdit edit ]
  else
    Some
      (Text_document.workspace_edit
         text_document
         [ TextEdit.create ~range:insert_range ~newText:("let " ^ name ^ binding_suffix)
         ; TextEdit.create ~range ~newText:(name ^ call_suffix)
         ])
;;

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
      | Texp_let (_, _, body) | Texp_while (_, body) | Texp_for (_, _, _, _, _, body) ->
        found_if_expr_contains body
      | Texp_letop { body; _ } -> found_if_case_contains [ body ]
      | Texp_function (_, Tfunction_cases { cases; _ }) -> found_if_case_contains cases
      | Texp_match _ ->
        let m = Typedtree_utils.texp_match_of_expr expr |> Option.value_exn in
        found_if_case_contains m.computation_cases
      | Texp_try _ ->
        let t = Typedtree_utils.texp_try_of_expr expr |> Option.value_exn in
        found_if_case_contains t.value_cases
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

let constructors_available (expr : Typedtree.expression) destination_env =
  let module I = Ocaml_typing.Tast_iterator in
  let exception Unavailable in
  let resolves_to env lid uid =
    match Env.find_constructor_by_name lid env with
    | constructor -> Types.Uid.equal uid constructor.cstr_uid
    | exception Not_found -> false
  in
  let check lid uid =
    (* If this constructor does not resolve before entering the selected expression,
       its scope is contained in the expression and moves with it. *)
    if resolves_to expr.exp_env lid uid && not (resolves_to destination_env lid uid)
    then raise_notrace Unavailable
  in
  let expr_iter (iter : I.iterator) (expr : Typedtree.expression) =
    (match expr.exp_desc with
     | Texp_construct (lid, constructor, _) -> check lid.txt constructor.cstr_uid
     | _ -> ());
    I.default_iterator.expr iter expr
  in
  let pat_iter (type k) (iter : I.iterator) (pat : k Typedtree.general_pattern) =
    (match pat.pat_desc with
     | Tpat_construct (lid, constructor, _, _) -> check lid.txt constructor.cstr_uid
     | _ -> ());
    I.default_iterator.pat iter pat
  in
  let iterator = { I.default_iterator with expr = expr_iter; pat = pat_iter } in
  try
    iterator.expr iterator expr;
    true
  with
  | Unavailable -> false
;;

let extract_local doc typedtree range =
  let* to_extract = largest_enclosed_expression typedtree range in
  let* extract_range = Range.of_loc_opt to_extract.exp_loc in
  let* local_text = Text_document.substring (Document.text_document doc) extract_range in
  let+ edit_pos = tightest_enclosing_binder_position typedtree range in
  { range = extract_range
  ; insert_range = Range.create ~start:edit_pos ~end_:edit_pos
  ; name = "var_name"
  ; binding_suffix = " = " ^ local_text ^ " in\n"
  ; call_suffix = ""
  }
;;

let extract_function doc typedtree range =
  let* to_extract = largest_enclosed_expression typedtree range in
  let* extract_range = Range.of_loc_opt to_extract.exp_loc in
  let* parent_item = enclosing_structure_item typedtree range in
  let* () = Option.some_if (constructors_available to_extract parent_item.str_env) () in
  let* edit_pos = Position.of_lexical_position parent_item.str_loc.loc_start in
  let* args_str =
    let free_vars = must_pass to_extract parent_item.str_env in
    let+ args =
      List.map free_vars ~f:(function
        | Longident.Lident id -> Some id
        | _ -> None)
      |> Option.all
    in
    let s = String.concat ~sep:" " args in
    if String.is_empty s then "()" else s
  in
  let+ func_text = Text_document.substring (Document.text_document doc) extract_range in
  { range = extract_range
  ; insert_range = Range.create ~start:edit_pos ~end_:edit_pos
  ; name = "fun_name"
  ; binding_suffix = " " ^ args_str ^ " = " ^ func_text ^ "\n\n"
  ; call_suffix = " " ^ args_str
  }
;;

let run_extract_local state pipeline doc (params : CodeActionParams.t) =
  let* extraction =
    let* typedtree =
      match Mpipeline.typer_result pipeline |> Mtyper.get_typedtree with
      | `Interface _ -> None
      | `Implementation x -> Some x
    in
    extract_local doc typedtree params.range
  in
  let+ edit = workspace_edit state doc extraction in
  CodeAction.create
    ~title:"Extract local"
    ~kind:CodeActionKind.RefactorExtract
    ~edit
    ~isPreferred:false
    ()
;;

let run_extract_function state pipeline doc (params : CodeActionParams.t) =
  let* extraction =
    let* typedtree =
      match Mpipeline.typer_result pipeline |> Mtyper.get_typedtree with
      | `Interface _ -> None
      | `Implementation x -> Some x
    in
    extract_function doc typedtree params.range
  in
  let+ edit = workspace_edit state doc extraction in
  CodeAction.create
    ~title:"Extract function"
    ~kind:CodeActionKind.RefactorExtract
    ~edit
    ~isPreferred:false
    ()
;;

let local state = Code_action.batchable RefactorExtract (run_extract_local state)
let function_ state = Code_action.batchable RefactorExtract (run_extract_function state)
