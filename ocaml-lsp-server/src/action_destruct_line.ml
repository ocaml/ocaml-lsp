open Import
open Fiber.O
open Core

let action_kind = "destruct-line (enumerate cases, use existing match)"
let kind = CodeActionKind.Other action_kind

(* TODO: All of the pre- and post-processing here is done by simple regexes and other
   string manipulations. It would be nice if more of it could rely on the typed tree or
   other analysis of the code provided by Merlin. *)

type statement_kind =
  | MatchLine (* [match ...] *)
  | MatchWithLine (* [match ... with] *)
  | CaseLine (* [|...->...] *)
  | Hole (* [|..._...->...] AND the range indicates a query at the underscore. *)
  | OffsetHole of int (* [| _ ->...] BUT the hole is here, not at the query location. *)

type destructable_statement =
  { code : string
  ; kind : statement_kind
  ; query_range : Range.t (* Range sent to Merlin based on our pre-processing. *)
  ; reply_range : Range.t (* Where Merlin's reply will go. *)
  }

(** Extracts the line of [doc] that the query indicated by [range] starts on. *)
let get_line (doc : Document.t) (range : Range.t) =
  let text = Document.text doc in
  let start_line = range.start.line + 1 in
  let source = Document.source doc in
  let (`Offset pos) = Msource.get_offset source (`Logical (start_line, 0)) in
  let (`Offset next) = Msource.get_offset source (`Logical (start_line + 1, 0)) in
  let len = next - pos in
  String.sub text ~pos ~len
;;

(** Assumes [case_line] passes the check for a CaseLine, but hasn't had whitespace
    removed. Checks that the cursor is before the arrow and the position before or after
    the cursor has an underscore. *)
let is_hole (case_line : string) (cursor_pos : int) =
  let arrow_pos = String.substr_index_exn case_line ~pattern:"->" in
  if cursor_pos <= 0 || cursor_pos >= arrow_pos
  then false (* We're only looking for '_' if the cursor is between "|" and "->". *)
  else if Char.equal case_line.[cursor_pos] '_'
          || Char.equal case_line.[cursor_pos - 1] '_'
  then true
  else false
;;

(** Finds the index of a lhs underscore in [case_line], if any. *)
let find_hole (case_line : string) =
  let start_of_lhs = 1 + String.substr_index_exn case_line ~pattern:"|" in
  let end_of_lhs = String.substr_index_exn case_line ~pattern:"->" in
  let lhs =
    String.strip (String.sub case_line ~pos:start_of_lhs ~len:(end_of_lhs - start_of_lhs))
  in
  if String.equal "_" lhs then String.substr_index case_line ~pattern:"_" else None
;;

let get_statement_kind (code_line : string) (range : Range.t) =
  let logical_line = String.strip code_line in
  (* Line contains [match] and [with], and has at least one word in between. *)
  let match_with_regex = ".*match[ \t]+[^ \t].*[ \t]with.*" in
  (* Line contains [match] followed by at least one other word. *)
  let match_regex = ".*match[ \t]+[^ \t]" in
  (* Line starts with a pipe and contains an arrow. *)
  let case_regex = "^\\|.*->.*" in
  let regex =
    Re2.Multiple.create_exn
      [ match_with_regex, `MatchWithLine; match_regex, `MatchLine; case_regex, `CaseLine ]
  in
  match Re2.Multiple.matches regex logical_line with
  | `MatchWithLine :: _ -> Some MatchWithLine
  | `MatchLine :: _ -> Some MatchLine
  | `CaseLine :: _ ->
    if is_hole code_line range.start.character
    then Some Hole
    else (
      match find_hole code_line with
      | None -> Some CaseLine
      | Some offset -> Some (OffsetHole offset))
  | [] -> None
;;

(** Given a line of the form [match x] or [match x with] or [| x -> y], create a query
    range corresponding to [x]. *)
let get_query_range (code : string) (kind : statement_kind) (range : Range.t) : Range.t =
  let start_char =
    match kind with
    | MatchLine | MatchWithLine -> 6 + String.substr_index_exn ~pattern:"match" code
    | CaseLine -> 2 + String.substr_index_exn ~pattern:"|" code
    | Hole -> range.start.character
    | OffsetHole c -> c
  in
  let end_char =
    match kind with
    | MatchLine -> String.length code - 1
    | MatchWithLine -> String.substr_index_exn ~pattern:"with" code - 1
    | CaseLine -> String.substr_index_exn code ~pattern:"->" - 1
    | Hole -> range.end_.character
    | OffsetHole c -> c
  in
  { start = { range.start with character = start_char }
  ; end_ = { range.end_ with character = end_char }
  }
;;

(** Finds the portion of the text that will be overwritten by Merlin's reply. For a
    MatchLine or a MatchWithLine, Merlin's reply will include "match" and "with", so to
    avoid duplication, we want the existing "match" and (possibly) "with" to be included
    in the range that gets replaced. *)
let get_reply_range (code : string) (kind : statement_kind) (query_range : Range.t)
  : Range.t
  =
  let start_char =
    match kind with
    | CaseLine | Hole | OffsetHole _ -> query_range.start.character
    | MatchLine | MatchWithLine -> String.substr_index_exn ~pattern:"match" code
  in
  let end_char =
    match kind with
    | MatchLine | CaseLine | Hole | OffsetHole _ -> query_range.end_.character
    | MatchWithLine -> 4 + String.substr_index_exn ~pattern:"with" code
  in
  { start = { query_range.start with character = start_char }
  ; end_ = { query_range.end_ with character = end_char }
  }
;;

(** Adjusts the location Merlin gave us to ensure the right text gets overwritten. *)
let adjust_reply_location ~(statement : destructable_statement) (loc : Loc.t) : Loc.t =
  let start_offset =
    statement.reply_range.start.character - statement.query_range.start.character
  in
  let end_offset =
    statement.reply_range.end_.character - statement.query_range.end_.character
  in
  let loc_start =
    { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + start_offset }
  in
  let loc_end = { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + end_offset } in
  { loc with loc_start; loc_end }
;;

(** Tries to find a statement we know how to handle on the line where the range starts. *)
let extract_statement (doc : Document.t) (ca_range : Range.t)
  : destructable_statement option
  =
  if ca_range.start.line <> ca_range.end_.line
  then None
  else (
    let code = get_line doc ca_range in
    match get_statement_kind code ca_range with
    | None -> None
    | Some kind ->
      let query_range = get_query_range code kind ca_range in
      let reply_range = get_reply_range code kind query_range in
      Some { code; kind; query_range; reply_range })
;;

(** Strips " -> ... " off the rhs and " | " off the lhs of a case-line if present. *)
let strip_case_line line =
  let line = String.strip line |> String.chop_prefix_if_exists ~prefix:"|" in
  let line =
    match String.substr_index line ~pattern:"->" with
    | None -> line
    | Some offset -> String.sub line ~pos:0 ~len:offset
  in
  String.strip line
;;

(** [count_surrounding_parens "((( abc ())) (de) )"] returns [(3,1)]. *)
let count_surrounding_parens text =
  let not_open _ c = Char.(c <> '(') in
  let not_close _ c = Char.(c <> ')') in
  let len = String.length text in
  let num_opens = String.lfindi text ~f:not_open |> Option.value ~default:len in
  let num_closes =
    len - 1 - (String.rfindi text ~f:not_close |> Option.value ~default:(-1))
  in
  num_opens, num_closes
;;

let strip_matched_parens text =
  let num_opens, num_closes = count_surrounding_parens text in
  let num_matched = min num_opens num_closes in
  String.sub text ~pos:num_matched ~len:(String.length text - (2 * num_matched))
;;

let remove_newlines text =
  String.map text ~f:(function
    | '\n' -> ' '
    | c -> c)
;;

(** Combines match-case lines, stripping leading '(' characters, and keeping count of how
    many more '(' have been removed than ')', in order to strip trailing ')' characters
    only if the match matching '(' characters were already stripped.

    Gives an error if it strips too many '(' characters and can't match them. *)
let format_match_cases lines ~indent =
  let extra_opens = ref 0 in
  let extract_match_case l =
    let l = strip_case_line l in
    let num_opens, num_closes = count_surrounding_parens l in
    let closes_to_strip = min (num_opens + !extra_opens) num_closes in
    let l =
      String.sub l ~pos:num_opens ~len:(String.length l - num_opens - closes_to_strip)
    in
    extra_opens := !extra_opens + num_opens - closes_to_strip;
    l
  in
  let lines =
    List.filter_map lines ~f:(fun l ->
      let l = remove_newlines l in
      match extract_match_case l with
      | "" -> None
      | l -> Some (indent ^ "| " ^ l ^ " -> _"))
  in
  match !extra_opens with
  | 0 -> Ok ("\n" ^ String.concat lines ~sep:"\n")
  | _ -> Error "Stripped too many open-parens."
;;

(** Finds the "with" in the Merlin reply and splits after it. *)
let separate_match_line new_code =
  let end_of_match = String.substr_index_exn new_code ~pattern:"with" in
  let match_line = String.prefix new_code (end_of_match + 4) in
  let rest = String.drop_prefix new_code (end_of_match + 4) in
  match_line, rest
;;

let format_merlin_reply ~(statement : destructable_statement) (new_code : string) =
  let indent = String.take_while statement.code ~f:Char.is_whitespace in
  match statement.kind with
  | MatchLine | MatchWithLine ->
    let new_code = strip_matched_parens new_code in
    let match_line, rest = separate_match_line new_code in
    let lines = String.split ~on:'|' rest in
    (match format_match_cases lines ~indent with
     | Ok case_lines -> match_line ^ case_lines
     | Error _ -> new_code)
  | CaseLine ->
    (match format_match_cases (String.split new_code ~on:'|') ~indent with
     | Ok case_line -> case_line
     | Error _ -> new_code)
  | Hole | OffsetHole _ ->
    let lines = String.split ~on:'|' new_code in
    (match List.hd lines, List.tl lines with
     | None, _ | _, None -> new_code
     | Some first_line, Some other_lines ->
       let other_lines =
         List.map other_lines ~f:(fun l ->
           indent ^ "| " ^ strip_case_line (remove_newlines l))
       in
       String.concat ~sep:" -> _\n" (String.strip first_line :: other_lines))
;;

let dispatch_destruct ~log_info (merlin : Document.Merlin.t) (range : Range.t) =
  let command =
    let start = Position.logical range.start in
    let finish = Position.logical range.end_ in
    Query_protocol.Case_analysis (start, finish)
  in
  Document.Merlin.dispatch ~log_info merlin command
;;

let code_action
  ~log_info
  (state : State.t)
  (doc : Document.t)
  (params : CodeActionParams.t)
  =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    (match Document.Merlin.kind merlin with
     | Intf -> Fiber.return None
     | Impl ->
       (match extract_statement doc params.range with
        | None -> Fiber.return None
        | Some statement ->
          let+ res = dispatch_destruct ~log_info merlin statement.query_range in
          (match res with
           | Ok (loc, newText) ->
             let loc = adjust_reply_location ~statement loc in
             let newText = format_merlin_reply ~statement newText in
             let range = Range.of_loc loc in
             let edit = Document.edit doc [ { range; newText } ] in
             let command = Typed_hole.next_hole_cmd ~state ~edit:{ range; newText } in
             let title = String.capitalize action_kind in
             let kind = CodeActionKind.Other action_kind in
             let action =
               CodeAction.create ~title ~kind ~edit ?command ~isPreferred:false ()
             in
             Some action
           | Error
               { exn =
                   ( Merlin_analysis.Destruct.Wrong_parent _
                   | Query_commands.No_nodes
                   | Merlin_analysis.Destruct.Not_allowed _
                   | Merlin_analysis.Destruct.Useless_refine
                   | Merlin_analysis.Destruct.Ill_typed
                   | Merlin_analysis.Destruct.Nothing_to_do )
               ; backtrace = _
               } -> None
           | Error exn -> Exn_with_backtrace.reraise exn)))
;;

let t state = { Code_action.kind; run = `Non_batchable (code_action state) }
