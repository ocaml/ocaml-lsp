open Import

let action_kind = "wrap-type-in-module"

(** Gets the type definition surrounding the cursor position if the cursor is within a
    type definition. *)
let type_definition_at pipeline pos_start =
  let pos_start = Mpipeline.get_lexing_pos pipeline pos_start in
  let typer = Mpipeline.typer_result pipeline in
  let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
  let scopes = Mbrowse.enclosing pos_start [ browse ] in
  List.find_map scopes ~f:(function
    | _, Type_declaration type_decl -> Some type_decl
    | _ -> None)
;;

(* Gets the portions of an outer interval that surround an inner subinterval. In an
   analogous case with just character positions, surrounding_portions ~outer:[0:5]
   ~inner:[2:3] = ([0, 1], [4, 5]).

   [inner] must be strictly contained within [outer]. This will always be the case with
   the type name of a type declaration. *)
let surrounding_portions
      ~(inner : Merlin_parsing.Location.t)
      ~(outer : Merlin_parsing.Location.t)
  =
  let before : Merlin_parsing.Location.t =
    { loc_start = outer.loc_start
    ; loc_end = inner.loc_start
    ; loc_ghost = outer.loc_ghost || inner.loc_ghost
    }
  in
  let after : Merlin_parsing.Location.t =
    { loc_start = inner.loc_end
    ; loc_end = outer.loc_end
    ; loc_ghost = outer.loc_ghost || inner.loc_ghost
    }
  in
  before, after
;;

let leading_whitespace doc (loc : Loc.t) =
  let before : Loc.t =
    { loc_start = { loc.loc_start with pos_cnum = 0 }
    ; loc_end = loc.loc_start
    ; loc_ghost = false
    }
  in
  Document.get_source_text doc before
;;

let new_module_text doc (type_decl : Ocaml_typing.Typedtree.type_declaration) =
  let open Option.O in
  let before, after =
    surrounding_portions ~inner:type_decl.typ_name.loc ~outer:type_decl.typ_loc
  in
  let* before_text = Document.get_source_text doc before in
  let* after_text = Document.get_source_text doc after in
  let* original_indent = leading_whitespace doc type_decl.typ_loc in
  let new_type_decl = before_text ^ "t" ^ after_text in
  let indented_type_decl =
    (* The type_decl is unevenly indented because the first line is unindented. This is
       because the type_decl's location doesn't necessarily start at column 0. *)
    original_indent ^ new_type_decl
    |> String.split_lines
    |> List.map ~f:(fun s -> "  " ^ s)
    |> String.concat ~sep:"\n"
  in
  let module_name = String.capitalize_ascii type_decl.typ_name.txt in
  match Document.kind doc with
  | `Merlin m ->
    let assign =
      match Document.Merlin.kind m with
      | Document.Kind.Intf -> ": sig"
      | Document.Kind.Impl -> "= struct"
    in
    Some
      (String.concat
         ~sep:"\n"
         [ (* Don't indent the first line because the edit starts at the original "t" in
              "type". *)
           Printf.sprintf "module %s %s" module_name assign
         ; indented_type_decl
         ; original_indent ^ "end"
         ])
  | `Other -> None
;;

let code_action pipeline doc (params : CodeActionParams.t) =
  let open Option.O in
  let pos_start = Position.logical params.range.start in
  let* type_decl = type_definition_at pipeline pos_start in
  let type_name = type_decl.typ_name.txt in
  if String.equal type_name "t"
  then None
  else
    let* newText = new_module_text doc type_decl in
    let uri = params.textDocument.uri in
    let edit : WorkspaceEdit.t =
      let textedit : TextEdit.t = { range = Range.of_loc type_decl.typ_loc; newText } in
      let version = Document.version doc in
      let textDocument =
        OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
      in
      let edit = TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ] in
      WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
    in
    let title = String.capitalize_ascii action_kind in
    Some
      (CodeAction.create
         ~title
         ~kind:(CodeActionKind.Other action_kind)
         ~edit
         ~isPreferred:false
         ())
;;

let kind = CodeActionKind.Other action_kind
let t = Code_action.batchable kind code_action
