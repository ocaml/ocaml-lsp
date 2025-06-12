open Import
open Core
open Option.Let_syntax

let action_kind = "combine-cases"
let kind = CodeActionKind.Other action_kind

(** If the range spans multiple lines, then extend it to ensure it captures the full first
    and last lines. *)
let select_complete_lines (range : Range.t) : Range.t option =
  if range.start.line = range.end_.line
  then None
  else (
    let start = { range.start with character = 0 } in
    match range.end_.character with
    | 0 -> Some { range with start }
    | _ ->
      let end_ : Position.t = { line = range.end_.line + 1; character = 0 } in
      Some { start; end_ })
;;

(** {v
 If each line of [code] is a match-statement case of the form
    "| lhs-pattern -> rhs-expression"
    then return two lists containing all the lhs-patterns and all the rhs-expressions.
    v} *)
let split_cases code =
  let lines = String.split code ~on:'\n' in
  (* Matches a line that starts with "|" and contains "->". *)
  let case_regex = Re2.create_exn "^\\s*\\|.*->" in
  let drop_from_lines lines regex =
    List.map lines ~f:(Re2.replace_exn regex ~f:(Fn.const ""))
  in
  match List.for_all ~f:(Re2.matches case_regex) lines with
  | false -> None
  | true ->
    (* Removes a leading "|" (with possible surrounding whitespace). *)
    let without_pipes = drop_from_lines lines (Re2.create_exn "^\\s*\\|\\s*") in
    (* Removes everything after the first "->" (with possible leading whitespace). *)
    let lhs_patterns = drop_from_lines without_pipes (Re2.create_exn "\\s*->.*$") in
    (* Removes everything before the first "->" (with possible trailing whitespace). *)
    let rhs_expressions = drop_from_lines without_pipes (Re2.create_exn "^.*->\\s*") in
    Some (lhs_patterns, rhs_expressions)
;;

(** If there is exactly one non-empty expression, return it; otherwise return a hole. *)
let pick_rhs rhs_expressions =
  let distinct_nonempty =
    List.map rhs_expressions ~f:String.strip
    |> List.filter ~f:(fun s -> (not (String.is_empty s)) && not (String.equal s "_"))
    |> List.dedup_and_sort ~compare:String.compare
  in
  match distinct_nonempty with
  | expr :: [] -> expr
  | _ -> "_"
;;

(** Called by the combine-cases code action. *)
let code_action ~log_info:_ (doc : Document.t) (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    (match Document.Merlin.kind merlin with
     | Intf -> Fiber.return None
     | Impl ->
       Fiber.return
         (let%bind range = select_complete_lines params.range in
          let%bind code = Document.substring doc range in
          let code = String.strip ~drop:(fun c -> Char.equal c '\n') code in
          let%bind lhs_patterns, rhs_expressions = split_cases code in
          let%bind i = String.index code '|' in
          let indent = String.sub code ~pos:0 ~len:i in
          let lhs = String.concat ~sep:" | " lhs_patterns in
          let rhs = pick_rhs rhs_expressions in
          let newText = [%string "%{indent}| %{lhs} -> %{rhs}\n"] in
          let edit = Document.edit doc [ { range; newText } ] in
          let title = String.capitalize action_kind in
          let action = CodeAction.create ~title ~kind ~edit ~isPreferred:false () in
          Some action))
;;

let t = { Code_action.kind; run = `Non_batchable code_action }
