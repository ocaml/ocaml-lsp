open Import

let action_kind = "combine-cases"
let kind = CodeActionKind.Other action_kind

let select_complete_lines (range : Range.t) =
  if range.start.line = range.end_.line
  then None
  else (
    let start = { range.start with character = 0 } in
    match range.end_.character with
    | 0 -> Some { range with start }
    | _ ->
      let end_ = Position.{ line = range.end_.line + 1; character = 0 } in
      Some (Range.create ~start ~end_))
;;

let split_cases code =
  let lines = String.split code ~on:'\n' in
  let case_regex = Re.Perl.re {|^\s*\|.*->|} |> Re.compile in
  let drop_from_lines lines regex =
    List.map lines ~f:(Re.replace_string (Re.compile regex) ~by:"")
  in
  match List.for_all ~f:(Re.execp case_regex) lines with
  | false -> None
  | true ->
    let without_pipes = drop_from_lines lines (Re.Perl.re {|\s*\|\s*|}) in
    let lhs_patterns = drop_from_lines without_pipes (Re.Perl.re {|\s*->.*$|}) in
    let rhs_expressions = drop_from_lines without_pipes (Re.Perl.re {|^.*->\s*|}) in
    Some (lhs_patterns, rhs_expressions)
;;

let pick_rhs rhs_expressions =
  let distinct_nonempty =
    List.map rhs_expressions ~f:String.strip
    |> List.filter ~f:(fun s -> (not (String.is_empty s)) && not (String.equal s "_"))
    |> Base.List.dedup_and_sort ~compare:Base.String.compare
  in
  match distinct_nonempty with
  | [ expr ] -> expr
  | _ -> "_"
;;

let make_text_edit ~range ~newText ~doc ~uri =
  let text_edit = TextEdit.create ~range ~newText in
  let version = Document.version doc in
  let textDocument = OptionalVersionedTextDocumentIdentifier.create ~uri ~version () in
  let edit = TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit text_edit ] in
  WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
;;

let code_action doc params =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    (match Document.Merlin.kind merlin with
     | Intf -> Fiber.return None
     | Impl ->
       let result =
         let open Option.O in
         let* range = select_complete_lines params.CodeActionParams.range in
         let* code = Document.substring doc range in
         let code = String.strip ~drop:(fun c -> Char.equal c '\n') code in
         let* lhs_patterns, rhs_expressions = split_cases code in
         let+ i = Base.String.index code '|' in
         let indent = String.sub code ~pos:0 ~len:i in
         let lhs = String.concat ~sep:" | " lhs_patterns in
         let rhs = pick_rhs rhs_expressions in
         let newText = indent ^ "| " ^ lhs ^ " -> " ^ rhs ^ "\n" in
         let edit = make_text_edit ~range ~newText ~doc ~uri:params.textDocument.uri in
         CodeAction.create
           ~title:(String.capitalize action_kind)
           ~kind
           ~edit
           ~isPreferred:false
           ()
       in
       Fiber.return result)
;;

let t = { Code_action.kind; run = `Non_batchable code_action }
