open Import
open Fiber.O

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

let trim_offsets str ~start ~stop =
  let start = ref start in
  while !start < stop && Base.Char.is_whitespace str.[!start] do
    incr start
  done;
  let stop = ref stop in
  while !stop > !start && Base.Char.is_whitespace str.[!stop - 1] do
    decr stop
  done;
  !start, !stop
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

let byte_offset_of_character ~position_encoding code character =
  Position.absolute_offset ~position_encoding code (Position.create ~line:0 ~character)
;;

let character_of_byte_offset ~position_encoding code offset =
  (Position.of_offset ~position_encoding code offset).character
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
  fun ~position_encoding (code_line : string) (range : Range.t) ->
    let logical_line = String.strip code_line in
    (* Line starts with [match], ends with [with], and has at least one other word. *)
    List.find_map regexes ~f:(fun (re, name) ->
      Option.some_if (Re.execp re logical_line) name)
    |> Option.bind ~f:(function
      | `MatchWithLine -> Some MatchWithLine
      | `MatchLine -> Some MatchLine
      | `CaseLine ->
        let cursor_pos =
          byte_offset_of_character ~position_encoding code_line range.start.character
        in
        if is_hole code_line cursor_pos
        then Some Hole
        else (
          match find_hole code_line with
          | None -> Some CaseLine
          | Some offset -> Some (OffsetHole offset)))
;;

(** Given a line of the form [match x] or [match x with] or [| x -> y], create a
    query range corresponding to [x]. *)
let get_query_range
      ~position_encoding
      (code : string)
      (kind : statement_kind)
      (range : Range.t)
  : Range.t
  =
  let start_character, end_character =
    match kind with
    | Hole -> range.start.character, range.end_.character
    | OffsetHole offset ->
      let character = character_of_byte_offset ~position_encoding code offset in
      character, character
    | MatchLine | MatchWithLine | CaseLine ->
      let logical_start, logical_end =
        trim_offsets code ~start:0 ~stop:(String.length code)
      in
      let start_offset, end_offset =
        match kind with
        | MatchLine -> trim_offsets code ~start:(logical_start + 5) ~stop:logical_end
        | MatchWithLine ->
          trim_offsets code ~start:(logical_start + 5) ~stop:(logical_end - 4)
        | CaseLine ->
          let arrow = String.substr_index_exn code ~pattern:"->" in
          trim_offsets code ~start:(logical_start + 1) ~stop:arrow
        | Hole | OffsetHole _ -> assert false
      in
      ( character_of_byte_offset ~position_encoding code start_offset
      , character_of_byte_offset ~position_encoding code end_offset )
  in
  { start = { range.start with character = start_character }
  ; end_ = { range.end_ with character = end_character }
  }
;;

(** Finds the portion of the text that will be overwritten by Merlin's reply.
    For a MatchLine or a MatchWithLine, Merlin's reply will include "match" and
    "with", so to avoid duplication, we want the existing "match" and (possibly)
    "with" to be included in the range that gets replaced. *)
let get_reply_range
      ~position_encoding
      (code : string)
      (kind : statement_kind)
      (query_range : Range.t)
  =
  match kind with
  | CaseLine | Hole | OffsetHole _ -> query_range
  | MatchLine | MatchWithLine ->
    let start_offset, end_offset =
      trim_offsets code ~start:0 ~stop:(String.length code)
    in
    let start_character = character_of_byte_offset ~position_encoding code start_offset in
    let end_character = character_of_byte_offset ~position_encoding code end_offset in
    { start = { query_range.start with character = start_character }
    ; end_ = { query_range.end_ with character = end_character }
    }
;;

(** Adjusts the location Merlin gave us to ensure the right text gets
    overwritten. *)
let adjust_reply_location
      ~position_encoding
      ~(statement : destructable_statement)
      (loc : Loc.t)
  : Loc.t
  =
  let byte_offset position =
    byte_offset_of_character ~position_encoding statement.code position.Position.character
  in
  let start_offset =
    byte_offset statement.reply_range.start - byte_offset statement.query_range.start
  in
  let end_offset =
    byte_offset statement.reply_range.end_ - byte_offset statement.query_range.end_
  in
  let loc_start =
    { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + start_offset }
  in
  let loc_end = { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + end_offset } in
  { loc with loc_start; loc_end }
;;

(** Tries to find a statement we know how to handle on the line where the range
    starts. *)
let extract_statement ~position_encoding (doc : Document.t) (ca_range : Range.t)
  : destructable_statement option
  =
  if ca_range.start.line <> ca_range.end_.line
  then None
  else (
    let code = get_line doc ca_range in
    match get_statement_kind ~position_encoding code ca_range with
    | None -> None
    | Some kind ->
      let query_range = get_query_range ~position_encoding code kind ca_range in
      let reply_range = get_reply_range ~position_encoding code kind query_range in
      Some { code; kind; query_range; reply_range })
;;

module For_tests = struct
  let ranges ~position_encoding ~code ~range =
    let open Option.O in
    let* kind = get_statement_kind ~position_encoding code range in
    let query_range = get_query_range ~position_encoding code kind range in
    let reply_range = get_reply_range ~position_encoding code kind query_range in
    Some (query_range, reply_range)
  ;;
end

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
  let rest = Base.String.drop_prefix new_code (end_of_match + 4) in
  match_line, rest
;;

let format_merlin_reply ~(statement : destructable_statement) (new_code : string) =
  let indent =
    match
      String.lfindi statement.code ~f:(fun _ c -> not (Base.Char.is_whitespace c))
    with
    | None -> ""
    | Some i -> String.sub statement.code ~pos:0 ~len:i
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

let code_action_of_case_analysis
      ~supportsJumpToNextHole
      ~position_encoding
      doc
      (loc, newText)
  =
  let range : Range.t =
    Range.of_loc_with_encoding ~position_encoding ~text:(Document.text doc) loc
  in
  let textedit : TextEdit.t = { range; newText } in
  let edit = Code_action.workspace_edit doc [ textedit ] in
  let title = String.capitalize action_kind in
  let command =
    if supportsJumpToNextHole
    then
      Some
        (Client.Custom_commands.next_hole
           ~in_range:(Range.resize_for_edit ~position_encoding textedit)
           ~notify_if_no_hole:false
           ())
    else None
  in
  CodeAction.create
    ~title
    ~kind:(CodeActionKind.Other action_kind)
    ~edit
    ?command
    ~isPreferred:false
    ()
;;

let dispatch_destruct
      ~position_encoding
      ~code
      (merlin : Document.Merlin.t)
      (range : Range.t)
  =
  let merlin_position (position : Position.t) =
    let character = byte_offset_of_character ~position_encoding code position.character in
    { position with character }
  in
  let command =
    let start = Position.logical (merlin_position range.start) in
    let finish = Position.logical (merlin_position range.end_) in
    Query_protocol.Case_analysis (start, finish)
  in
  Document.Merlin.dispatch ~name:"destruct" merlin command
;;

let code_action (state : State.t) (doc : Document.t) (params : CodeActionParams.t) =
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin ->
    let position_encoding = State.position_encoding state in
    (match
       Document.Merlin.kind merlin, extract_statement ~position_encoding doc params.range
     with
     | Intf, _ | _, None -> Fiber.return None
     | Impl, Some statement ->
       let+ res =
         dispatch_destruct
           ~position_encoding
           ~code:statement.code
           merlin
           statement.query_range
       in
       (match res with
        | Ok (loc, newText) ->
          let loc = adjust_reply_location ~position_encoding ~statement loc in
          let newText = format_merlin_reply ~statement newText in
          let supportsJumpToNextHole =
            State.experimental_client_capabilities state
            |> Client.Experimental_capabilities.supportsJumpToNextHole
          in
          Some
            (code_action_of_case_analysis
               ~supportsJumpToNextHole
               ~position_encoding
               doc
               (loc, newText))
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
        | Error exn -> Exn_with_backtrace.reraise exn))
;;

let t state = { Code_action.kind; run = `Non_batchable (code_action state) }
