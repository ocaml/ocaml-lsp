open Import

let action_kind = "combine-cases"
let kind = CodeActionKind.Other action_kind

let select_complete_lines (range : Range.t) =
  if Range.is_single_line range
  then None
  else (
    let start = { range.start with character = 0 } in
    match range.end_.character with
    | 0 -> Some { range with start }
    | _ ->
      let end_ = Position.{ line = range.end_.line + 1; character = 0 } in
      Some (Range.create ~start ~end_))
;;

module Lexer = Ocaml_preprocess.Lexer_raw
module Parser = Ocaml_preprocess.Parser_raw

let split_case line =
  let open Option.O in
  let* source = String.chop_prefix (String.strip line) ~prefix:"|" in
  let lexer = Lexer.make (Lexer.keywords []) in
  let rec token = function
    | Lexer.Return token -> token
    | Refill refill -> token (refill ())
    | Fail _ -> raise Parser.Error
  in
  let arrows = ref [] in
  let first = ref true in
  let next lexbuf =
    if !first
    then (
      first := false;
      Parser.FUNCTION)
    else (
      let result = token (Lexer.token_without_comments lexer lexbuf) in
      (match result with
       | Parser.MINUSGREATER ->
         arrows := (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf) :: !arrows
       | _ -> ());
      result)
  in
  match Parser.parse_expression next (Lexing.from_string source) with
  | { pexp_desc =
        Pexp_function
          ([], None, Pfunction_cases ([ { pc_lhs; pc_guard = None; _ } ], _, _))
    ; _
    } ->
    (* The case arrow follows the complete pattern, including any type arrows.
       Keep the source on both sides so comments and RHS functions survive. *)
    List.find_map (List.rev !arrows) ~f:(fun (start, stop) ->
      if start < pc_lhs.ppat_loc.loc_end.pos_cnum
      then None
      else
        Some
          ( String.strip (String.prefix source start)
          , String.strip (String.drop_prefix source stop) ))
  | _ -> None
  | exception (Parser.Error | Ocaml_parsing.Syntaxerr.Error _) -> None
;;

let split_cases code =
  let open Option.O in
  let+ cases = String.split code ~on:'\n' |> List.map ~f:split_case |> Option.all in
  List.unzip cases
;;

let pick_rhs rhs_expressions =
  let distinct_nonempty =
    List.map rhs_expressions ~f:String.strip
    |> List.filter ~f:(fun s -> (not (String.is_empty s)) && not (String.equal s "_"))
    |> List.dedup_and_sort ~compare:String.compare
  in
  match distinct_nonempty with
  | [ expr ] -> expr
  | _ -> "_"
;;

let make_text_edit ~range ~newText ~doc =
  let text_edit = TextEdit.create ~range ~newText in
  Text_document.workspace_edit (Document.text_document doc) [ text_edit ]
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
         let* code = Text_document.substring (Document.text_document doc) range in
         let code = String.strip ~drop:(fun c -> Char.equal c '\n') code in
         let* lhs_patterns, rhs_expressions = split_cases code in
         let+ i = String.index code '|' in
         let indent = String.prefix code i in
         let lhs = String.concat ~sep:" | " lhs_patterns in
         let rhs = pick_rhs rhs_expressions in
         let newText = indent ^ "| " ^ lhs ^ " -> " ^ rhs ^ "\n" in
         let edit = make_text_edit ~range ~newText ~doc in
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
