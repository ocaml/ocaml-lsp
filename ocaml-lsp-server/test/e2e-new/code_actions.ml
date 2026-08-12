open Test.Import
open Lsp_helpers

let range ~start_line ~start_character ~end_line ~end_character =
  let start = Position.create ~line:start_line ~character:start_character in
  let end_ = Position.create ~line:end_line ~character:end_character in
  Range.create ~start ~end_
;;

let iter_code_actions ?prep ?path ?capabilities ?(diagnostics = []) ?only ~source range =
  let makeRequest textDocument =
    let context = CodeActionContext.create ~diagnostics ?only () in
    Lsp.Client_request.CodeAction
      (CodeActionParams.create ~textDocument ~range ~context ())
  in
  iter_lsp_response ?prep ?path ?capabilities ~language_id:"ocaml" ~makeRequest ~source
;;

let print_code_action_result ?(filter = fun _ -> true) = function
  | None -> print_endline "No code actions"
  | Some code_actions ->
    code_actions
    |> List.filter ~f:filter
    |> (function
     | [] -> print_endline "No code actions"
     | actions ->
       print_endline "Code actions:";
       List.iter actions ~f:(fun ca ->
         let json =
           match ca with
           | `Command command -> Command.yojson_of_t command
           | `CodeAction ca -> CodeAction.yojson_of_t ca
         in
         Yojson.Safe.pretty_to_string ~std:false json |> print_endline))
;;

let print_code_actions
      ?(prep = fun _ -> Fiber.return ())
      ?(path = "foo.ml")
      ?(diagnostics = [])
      ?only
      ?filter
      ?capabilities
      source
      range
  =
  iter_code_actions
    ~prep
    ~path
    ~diagnostics
    ?only
    ?capabilities
    ~source
    range
    (print_code_action_result ?filter)
;;

let find_action action_name action =
  match action with
  | `CodeAction { CodeAction.kind = Some (Other name); _ } ->
    String.equal name action_name
  | _ -> false
;;

let find_annotate_action action = find_action "type-annotate" action
let find_remove_annotation_action action = find_action "remove type annotation" action

let position_of_offset src target =
  assert (0 <= target && target < String.length src);
  let rec loop offset line character =
    if offset = target
    then Position.create ~line ~character
    else (
      let decoded = Stdlib.String.get_utf_8_uchar src offset in
      assert (Stdlib.Uchar.utf_decode_is_valid decoded);
      let uchar = Stdlib.Uchar.utf_decode_uchar decoded in
      let byte_length = Stdlib.Uchar.utf_decode_length decoded in
      assert (offset + byte_length <= target);
      if Stdlib.Uchar.equal uchar (Stdlib.Uchar.of_char '\n')
      then loop (offset + byte_length) (line + 1) 0
      else
        loop
          (offset + byte_length)
          line
          (character + (Stdlib.Uchar.utf_16_byte_length uchar / 2)))
  in
  loop 0 0 0
;;

let parse_selection src =
  let start_pos =
    match String.index src '$' with
    | Some x -> x
    | None -> failwith "expected a selection opening mark"
  in
  let end_pos =
    match String.index_from src (start_pos + 1) '$' with
    | Some x ->
      if Option.is_some (String.index_from src (x + 1) '$')
      then failwith "unexpected third selection mark";
      x - 1 (* account for opening mark *)
    | None -> start_pos
  in
  let start = position_of_offset src start_pos in
  let end_ = position_of_offset src end_pos in
  let src' =
    String.filter_map src ~f:(function
      | '$' -> None
      | c -> Some c)
  in
  src', Range.create ~start ~end_
;;

let apply_code_action ?diagnostics title source range =
  let open Option.O in
  (* collect code action results *)
  let code_actions = ref None in
  iter_code_actions ?diagnostics ~source range (fun ca -> code_actions := Some ca);
  let* m_code_actions = !code_actions in
  let* code_actions = m_code_actions in
  let* edit =
    List.find_map code_actions ~f:(function
      | `CodeAction { title = t; edit = Some edit; _ } when t = title -> Some edit
      | _ -> None)
  in
  let+ changes = edit.documentChanges in
  List.concat_map changes ~f:(function
    | `TextDocumentEdit x ->
      List.map x.edits ~f:(function
        | `AnnotatedTextEdit (a : AnnotatedTextEdit.t) ->
          TextEdit.create ~newText:a.newText ~range:a.range
        | `SnippetTextEdit (s : SnippetTextEdit.t) ->
          TextEdit.create ~newText:s.snippet.value ~range:s.range
        | `TextEdit e -> e)
    | `CreateFile _ | `DeleteFile _ | `RenameFile _ -> [])
  |> Test.apply_edits source
;;

let code_action_test ?(print_none = false) ~title source =
  let src, range = parse_selection source in
  match apply_code_action title src range with
  | None -> if print_none then print_endline "None"
  | Some result -> print_string result
;;
