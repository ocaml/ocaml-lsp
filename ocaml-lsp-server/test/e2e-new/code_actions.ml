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
let parse_selection = Test.parse_selection

let apply_code_action ?prep ?path ?diagnostics title source range =
  let open Option.O in
  (* collect code action results *)
  let code_actions = ref None in
  iter_code_actions ?prep ?path ?diagnostics ~source range (fun ca ->
    code_actions := Some ca);
  let* m_code_actions = !code_actions in
  let* code_actions = m_code_actions in
  let+ edit =
    List.find_map code_actions ~f:(function
      | `CodeAction { title = t; edit = Some edit; _ } when t = title -> Some edit
      | _ -> None)
  in
  Test.apply_workspace_edit source edit
;;

let code_action_test ?prep ?path ?diagnostics ?(print_none = false) ~title source =
  let src, range = parse_selection source in
  match apply_code_action ?prep ?path ?diagnostics title src range with
  | None -> if print_none then print_endline "None"
  | Some result -> print_string result
;;
