open Import

let action_title = "Inline"

let log str =
  let ch =
    open_out_gen [ Open_wronly; Open_append ] 0o744 "/home/feser/ocamllsp.log"
  in
  output_string ch str;
  flush ch;
  close_out ch

type inline_task =
  { ident : Ident.t
  ; body : Typedtree.expression  (** the expression to inline *)
  ; context : Typedtree.expression  (** where to perform inlining *)
  }

let find_inline_task pipeline pos =
  let contains loc pos =
    match Position.compare_inclusion pos (Range.of_loc loc) with
    | `Outside _ -> false
    | `Inside -> true
  in

  (* Find most enclosing nonrecursive let binding *)
  let browse =
    Mpipeline.typer_result pipeline
    |> Mtyper.get_typedtree |> Mbrowse.of_typedtree
  in

  Mbrowse.enclosing
    (Mpipeline.get_lexing_pos pipeline (Position.logical pos))
    [ browse ]
  |> List.find_map ~f:(function
       | ( _
         , Browse_raw.Expression
             { exp_desc =
                 Texp_let
                   ( Nonrecursive
                   , [ { vb_pat = { pat_desc = Tpat_var (id, s); _ }
                       ; vb_expr
                       ; _
                       }
                     ]
                   , rhs )
             ; _
             } )
         when contains s.loc pos ->
         Some { ident = id; body = vb_expr; context = rhs }
       | _ -> None)

let iter_ident_locs id expr k =
  let expr_iter iter (expr : Typedtree.expression) =
    match expr.exp_desc with
    | Texp_ident (Pident id', { loc; _ }, _) when Ident.same id id' -> k loc
    | _ -> Ocaml_typing.Tast_iterator.default_iterator.expr iter expr
  in
  let iterator =
    { Ocaml_typing.Tast_iterator.default_iterator with expr = expr_iter }
  in
  iterator.expr iterator expr

(** Iterate over the inlining edits, one for each occurrence of the bound
    variable. *)
let iter_inline_edits task _doc k =
  let newText =
    Format.asprintf "(%a)" Ocaml_parsing.Pprintast.expression
      (Ocaml_typing.Untypeast.untype_expression task.body)
    (* let start = task.body.exp_loc.loc_start.pos_cnum in *)
    (* let end_ = task.body.exp_loc.loc_end.pos_cnum in *)
    (* "(" ^ String.sub (Document.text doc) ~pos:start ~len:(end_ - start) ^ ")" *)
  in
  iter_ident_locs task.ident task.context (fun loc ->
      let textedit = TextEdit.create ~newText ~range:(Range.of_loc loc) in
      k textedit)

module Test = struct
  let f x y = x + y

  let g y = f y y

  let h g x = g x

  let j x = h g x

  let test () =
    let y x = x + 1 in
    y 0 + 2
end

let code_action doc (params : CodeActionParams.t) =
  let open Fiber.O in
  let+ m_inline_task =
    Document.with_pipeline_exn doc (fun pipeline ->
        find_inline_task pipeline params.range.start)
  in
  Option.map m_inline_task ~f:(fun task ->
      let edits = Queue.create () in
      iter_inline_edits task doc (Queue.push edits);

      let edit =
        let version = Document.version doc in
        let textDocument =
          OptionalVersionedTextDocumentIdentifier.create
            ~uri:params.textDocument.uri ~version ()
        in
        let edit =
          TextDocumentEdit.create ~textDocument
            ~edits:(Queue.to_list edits |> List.map ~f:(fun e -> `TextEdit e))
        in
        WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
      in
      CodeAction.create ~title:action_title ~kind:CodeActionKind.RefactorInline
        ~edit ~isPreferred:false ())

let t = { Code_action.kind = RefactorInline; run = code_action }
