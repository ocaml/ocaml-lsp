open Import

module Search = struct
  module Lexer = Ocaml_preprocess.Lexer_raw
  module Parser = Ocaml_preprocess.Parser_raw

  type t =
    { match_start : int
    ; case_start : int option
    }

  type token =
    { kind : Parser.token
    ; start : int
    ; end_ : int
    }

  let lexer code =
    let lexbuf = Lexing.from_string code in
    let lexer = Lexer.make (Lexer.keywords []) in
    let rec finish = function
      | Lexer.Return token -> Some token
      | Refill refill -> finish (refill ())
      | Fail _ -> None
    in
    Staged.stage (fun () ->
      match finish (Lexer.token_without_comments lexer lexbuf) with
      | None | Some EOF -> None
      | Some kind ->
        Some { kind; start = Lexing.lexeme_start lexbuf; end_ = Lexing.lexeme_end lexbuf })
  ;;

  let find_case code ~start next =
    (* The stack tracks the innermost open constructs: [`Match] for a nested
     [match]/[try] and [`Brace] for a record-update brace. A [with] closes the
     innermost [`Match]; a [with] above a [`Brace] belongs to the record
     update; and a [with] with an empty stack belongs to the [match] we started
     from, whose first case (if any) immediately follows. *)
    (* The search is bounded to a few lines after the [match] to avoid lexing the
     rest of the file when the [match] has no cases yet. *)
    let max_lines = 100 in
    let lines = ref 0 in
    let prev_end = ref start in
    let budget_ok token =
      let len = token.start - !prev_end in
      if len > 0
      then (
        let skipped = String.sub code ~pos:!prev_end ~len in
        String.iter skipped ~f:(fun c -> if Char.equal c '\n' then incr lines));
      prev_end := token.end_;
      !lines <= max_lines
    in
    let rec loop stack =
      match next () with
      | None -> None
      | Some token when not (budget_ok token) -> None
      | Some { kind = MATCH | TRY; _ } -> loop (`Match :: stack)
      | Some { kind = LBRACE; _ } -> loop (`Brace :: stack)
      | Some { kind = RBRACE; _ } ->
        (match stack with
         | `Brace :: rest -> loop rest
         | _ -> loop stack)
      | Some { kind = WITH; _ } ->
        (match stack with
         | `Match :: rest -> loop rest
         | `Brace :: _ -> loop stack
         | [] ->
           (match next () with
            | Some ({ kind = BAR; start; _ } as token) when budget_ok token -> Some start
            | None | Some _ -> None))
      | Some _ -> loop stack
    in
    loop []
  ;;

  let find code ~position =
    let line_end =
      String.substr_index code ~pattern:"\n" |> Option.value ~default:(String.length code)
    in
    let next = Staged.unstage (lexer code) in
    match next () with
    | None -> None
    | Some first ->
      let rec loop token =
        if token.start >= line_end
        then None
        else (
          match token.kind with
          | MATCH
            when token.start = first.start
                 || (token.start <= position && position <= token.end_) ->
            Some
              { match_start = token.start
              ; case_start = find_case code ~start:token.end_ next
              }
          | MATCH | _ ->
            (match next () with
             | None -> None
             | Some token -> loop token))
      in
      loop first
  ;;
end

let action_kind = "destruct-line (enumerate cases, use existing match)"
let kind = CodeActionKind.Other action_kind

(* TODO: All of the pre- and post-processing here is done by simple regexes and other
   string manipulations. It would be nice if more of it could rely on the typed tree or
   other analysis of the code provided by Merlin. *)

type statement_kind =
  | MatchLine (* [match ...] *)
  | MatchWithLine (* [match ... with] *)
  | CaseLine (* [|...->...] *)
  | Hole
  (* [|..._...->...] AND the range indicates a query at the underscore. *)
  | OffsetHole of int (* [| _ ->...] BUT the hole is here, not at the query location. *)

type destructable_statement =
  { code : string
  ; kind : statement_kind
  ; query_range : Range.t (* Range sent to Merlin based on our pre-processing. *)
  ; reply_range : Range.t (* Where Merlin's reply will go. *)
  }

(** Extracts the line of [doc] that the query indicated by [range] starts on.*)
let get_line (doc : Document.t) (range : Range.t) =
  let text = Document.text doc in
  let start_line = range.start.line + 1 in
  let source = Document.source doc in
  let (`Offset pos) = Msource.get_offset source (`Logical (start_line, 0)) in
  let (`Offset next) = Msource.get_offset source (`Logical (start_line + 1, 0)) in
  let len = next - pos in
  String.sub text ~pos ~len
;;

(** Trims leading and trailing whitespace plus some number of additional
    characters from the head and tail of a string. Used to transform [match x]
    or [match x with] to [x]. *)
let strip_head_and_tail str ~head_offset ~tail_offset =
  let str = String.strip str in
  let l = String.length str in
  let substr = String.sub str ~pos:head_offset ~len:(l - head_offset - tail_offset) in
  String.strip substr
;;

(** Finds the start and end indices of a substring for extraction. *)
let substr_endpoints_exn ~str ~substr =
  let start_index = String.substr_index_exn str ~pattern:substr in
  let end_index = start_index + String.length substr in
  start_index, end_index
;;

(** Assumes [case_line] passes the check for a CaseLine, but hasn't had
    whitespace removed. Checks that the cursor is before the arrow and the
    position before or after the cursor has an underscore. *)
let is_hole (case_line : string) (cursor_pos : int) =
  let arrow_pos = String.substr_index_exn case_line ~pattern:"->" in
  if cursor_pos <= 0 || cursor_pos >= arrow_pos
  then false (* We're only looking for '_' if the cursor is between "|" and "->". *)
  else if
    Char.equal case_line.[cursor_pos] '_' || Char.equal case_line.[cursor_pos - 1] '_'
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

let get_statement_kind =
  let space_without_nl = Re.set " \t" in
  (* Line starts with [match] and has at least one other word. *)
  let match_regex =
    let open Re in
    seq [ str "match"; rep1 space_without_nl; compl [ space_without_nl ] ]
  in
  let match_with_regex =
    let open Re in
    seq [ match_regex; rep any; space_without_nl; str "with"; eos ]
  in
  (* Line starts with a pipe and contains an arrow. *)
  let case_regex =
    let open Re in
    seq [ str "|"; rep any; str "->"; rep any ]
  in
  let regexes =
    [ match_with_regex, `MatchWithLine; match_regex, `MatchLine; case_regex, `CaseLine ]
    |> List.map ~f:(fun (re, kind) -> Re.(seq [ bos; re ] |> compile), kind)
  in
  fun (code_line : string) (range : Range.t) ->
    let logical_line = String.strip code_line in
    (* Line starts with [match], ends with [with], and has at least one other word. *)
    List.find_map regexes ~f:(fun (re, name) ->
      Option.some_if (Re.execp re logical_line) name)
    |> Option.bind ~f:(function
      | `MatchWithLine -> Some MatchWithLine
      | `MatchLine -> Some MatchLine
      | `CaseLine ->
        if is_hole code_line range.start.character
        then Some Hole
        else (
          match find_hole code_line with
          | None -> Some CaseLine
          | Some offset -> Some (OffsetHole offset)))
;;

(** Given a line of the form [match x] or [match x with] or [| x -> y], create a
    query range corresponding to [x]. *)
let get_query_range (code : string) (kind : statement_kind) (range : Range.t) : Range.t =
  let expr =
    match kind with
    | MatchLine -> strip_head_and_tail code ~head_offset:5 ~tail_offset:0
    | MatchWithLine -> strip_head_and_tail code ~head_offset:5 ~tail_offset:4
    | CaseLine ->
      let len = String.substr_index_exn code ~pattern:"->" in
      let expr = String.prefix code len in
      strip_head_and_tail expr ~head_offset:1 ~tail_offset:0
    | Hole | OffsetHole _ -> ""
  in
  let start_index, end_index =
    match kind with
    | Hole -> range.start.character, range.end_.character
    | OffsetHole offset -> offset, offset
    | _ -> substr_endpoints_exn ~str:code ~substr:expr
  in
  { start = { range.start with character = start_index }
  ; end_ = { range.end_ with character = end_index }
  }
;;

(** Finds the portion of the text that will be overwritten by Merlin's reply.
    For a MatchLine or a MatchWithLine, Merlin's reply will include "match" and
    "with", so to avoid duplication, we want the existing "match" and (possibly)
    "with" to be included in the range that gets replaced. *)
let get_reply_range (code : string) (kind : statement_kind) (query_range : Range.t) =
  match kind with
  | CaseLine | Hole | OffsetHole _ -> query_range
  | MatchLine | MatchWithLine ->
    let logical_line = String.strip code in
    let start_char, end_char = substr_endpoints_exn ~str:code ~substr:logical_line in
    { start = { query_range.start with character = start_char }
    ; end_ = { query_range.end_ with character = end_char }
    }
;;

(** Adjusts the location Merlin gave us to ensure the right text gets
    overwritten. *)
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

let statement_of_code ~prefix_len code range =
  let code = String.make prefix_len ' ' ^ String.drop_prefix code prefix_len in
  match get_statement_kind code range with
  | None -> None
  | Some kind ->
    let query_range = get_query_range code kind range in
    let reply_range = get_reply_range code kind query_range in
    Some { code; kind; query_range; reply_range }
;;

let statement_at_offset doc source offset =
  let (`Logical (line, character)) = Msource.get_logical source (`Offset offset) in
  let position = Position.create ~line:(line - 1) ~character in
  let range = Range.create ~start:position ~end_:position in
  statement_of_code ~prefix_len:character (get_line doc range) range
;;

(** Tries to find a statement we know how to handle on the line where the range
    starts. Inline matches are focused by masking their prefix, preserving all
    character offsets used by the existing line-oriented processing. *)
let extract_statement (doc : Document.t) (ca_range : Range.t)
  : destructable_statement option
  =
  let multiline = not (Lsp.Range.is_single_line ca_range) in
  let line_range : Range.t =
    if multiline then { start = ca_range.start; end_ = ca_range.start } else ca_range
  in
  let code = get_line doc line_range in
  let source = Document.source doc in
  let (`Offset line_start) =
    Msource.get_offset source (`Logical (line_range.start.line + 1, 0))
  in
  let search_code = String.drop_prefix (Document.text doc) line_start in
  match Search.find search_code ~position:line_range.start.character with
  | None -> if multiline then None else statement_of_code ~prefix_len:0 code line_range
  | Some { case_start = Some case_start; _ } ->
    statement_at_offset doc source (line_start + case_start)
  | Some { match_start; case_start = None } ->
    statement_of_code ~prefix_len:match_start code line_range
;;

(** Strips " -> _ " off the rhs and " | " off the lhs of a case-line if present. *)
let strip_case_line line =
  String.strip line
  |> String.chop_prefix_if_exists ~prefix:"|"
  |> String.chop_suffix_if_exists ~suffix:"_"
  |> String.strip
  |> String.chop_suffix_if_exists ~suffix:"->"
  |> String.strip
;;

let strip_parens line =
  String.chop_prefix_if_exists line ~prefix:"("
  |> String.chop_suffix_if_exists ~suffix:")"
;;

(** Combines match-case lines that have already been stripped. *)
let format_match_cases lines ~indent =
  "\n"
  ^ (List.filter_map lines ~f:(fun l ->
       match strip_parens (strip_case_line l) with
       | "" -> None
       | l -> Some (indent ^ "| " ^ l ^ " -> _"))
     |> String.concat ~sep:"\n")
;;

(** Finds the "with" in the Merlin reply and splits after it. *)
let separate_match_line new_code =
  let end_of_match = String.substr_index_exn new_code ~pattern:"with" in
  let match_line = String.prefix new_code (end_of_match + 4) in
  let rest = String.drop_prefix new_code (end_of_match + 4) in
  match_line, rest
;;

let format_merlin_reply ~(statement : destructable_statement) (new_code : string) =
  let indent =
    match String.lfindi statement.code ~f:(fun _ c -> not (Char.is_whitespace c)) with
    | None -> ""
    | Some i -> String.prefix statement.code i
  in
  match statement.kind with
  | MatchLine | MatchWithLine ->
    let match_line, rest = separate_match_line new_code in
    let rest = String.chop_suffix_if_exists rest ~suffix:")" in
    let match_line = String.chop_prefix_if_exists match_line ~prefix:"(" in
    let lines = String.split ~on:'|' rest in
    match_line ^ format_match_cases lines ~indent
  | CaseLine -> format_match_cases (String.split ~on:'|' new_code) ~indent
  | Hole | OffsetHole _ ->
    let lines = String.split ~on:'|' new_code in
    (match List.hd lines, List.tl lines with
     | None, _ | _, None -> new_code
     | Some first_line, Some other_lines ->
       let other_lines =
         List.map other_lines ~f:(fun l -> indent ^ "| " ^ strip_case_line l)
       in
       String.concat ~sep:" -> _\n" (String.strip first_line :: other_lines))
;;

let code_action
      (state : State.t)
      dispatch
      (doc : Document.t)
      (params : CodeActionParams.t)
  =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    (match Document.Merlin.kind merlin, extract_statement doc params.range with
     | Intf, _ | _, None -> Fiber.return None
     | Impl, Some statement ->
       Action_destruct.run
         state
         doc
         ~dispatch
         ~action_kind
         ~range:statement.query_range
         ~postprocess:(fun (loc, newText) ->
           let loc = adjust_reply_location ~statement loc in
           let newText = format_merlin_reply ~statement newText in
           loc, newText))
;;

let t ~dispatch state =
  { Code_action.kind; run = `Non_batchable (code_action state dispatch) }
;;

module Testing = struct
  module Search = Search
end
