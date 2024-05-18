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

type destructable_statement =
  { code : string
  ; kind : statement_kind
  ; query_range : Range.t
        (* Range sent to Merlin based on our pre-processing. *)
  ; reply_range : Range.t (* Where Merlin's reply will go. *)
  }

(** Extracts the line of [doc] that the query indicated by [range] starts on.*)
let get_line (doc : Document.t) (range : Range.t) =
  let text = Document.text doc in
  let start_line = range.start.line + 1 in
  let source = Document.source doc in
  let (`Offset pos) = Msource.get_offset source (`Logical (start_line, 0)) in
  let (`Offset next) =
    Msource.get_offset source (`Logical (start_line + 1, 0))
  in
  let len = next - pos in
  String.sub text ~pos ~len

(** Trims leading and trailing whitespace plus some number of additional
    characters from the head and tail of a string. Used to transform [match x]
    or [match x with] to [x]. *)
let strip_head_and_tail str ~head_offset ~tail_offset =
  let str = String.strip str in
  let l = String.length str in
  let substr =
    String.sub str ~pos:head_offset ~len:(l - head_offset - tail_offset)
  in
  String.strip substr

(** Finds the start and end indices of a substring for extraction. *)
let substr_endpoints_exn ~str ~substr =
  let start_index = String.substr_index_exn str ~pattern:substr in
  let end_index = start_index + String.length substr in
  (start_index, end_index)

(** Assumes [case_line] passes the check for a CaseLine, but hasn't had
    whitespace removed. Checks that the cursor is before the arrow and the
    position before or after the cursor has an underscore. *)
let is_hole (case_line : string) (cursor_pos : int) =
  let arrow_pos = String.substr_index_exn case_line ~pattern:"->" in
  if cursor_pos <= 0 || cursor_pos >= arrow_pos then false
    (* We're only looking for '_' if the cursor is between "|" and "->". *)
  else if
    Char.equal case_line.[cursor_pos] '_'
    || Char.equal case_line.[cursor_pos - 1] '_'
  then true
  else false

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
    [ (match_with_regex, `MatchWithLine)
    ; (match_regex, `MatchLine)
    ; (case_regex, `CaseLine)
    ]
    |> List.map ~f:(fun (re, kind) -> (Re.(seq [ bos; re ] |> compile), kind))
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
             if is_hole code_line range.start.character then Some Hole
             else Some CaseLine)

(** Given a line of the form [match x] or [match x with] or [| x -> y], create a
    query range corresponding to [x]. *)
let get_query_range (code : string) (kind : statement_kind) (range : Range.t) :
    Range.t =
  let expr =
    match kind with
    | MatchLine -> strip_head_and_tail code ~head_offset:5 ~tail_offset:0
    | MatchWithLine -> strip_head_and_tail code ~head_offset:5 ~tail_offset:4
    | CaseLine ->
      let len = String.substr_index_exn code ~pattern:"->" in
      let expr = String.sub code ~pos:0 ~len in
      strip_head_and_tail expr ~head_offset:1 ~tail_offset:0
    | Hole -> ""
  in
  let start_index, end_index =
    match kind with
    | Hole -> (range.start.character, range.end_.character)
    | _ -> substr_endpoints_exn ~str:code ~substr:expr
  in
  { start = { range.start with character = start_index }
  ; end_ = { range.end_ with character = end_index }
  }

(** Finds the portion of the text that will be overwritten by Merlin's reply.
    For a MatchLine or a MatchWithLine, Merlin's reply will include "match" and
    "with", so to avoid duplication, we want the existing "match" and (possibly)
    "with" to be included in the range that gets replaced. *)
let get_reply_range (code : string) (kind : statement_kind)
    (query_range : Range.t) =
  match kind with
  | CaseLine | Hole -> query_range
  | MatchLine | MatchWithLine ->
    let logical_line = String.strip code in
    let start_char, end_char =
      substr_endpoints_exn ~str:code ~substr:logical_line
    in
    { start = { query_range.start with character = start_char }
    ; end_ = { query_range.end_ with character = end_char }
    }

(** Adjusts the location Merlin gave us to ensure the right text gets
    overwritten. *)
let adjust_reply_location ~(statement : destructable_statement) (loc : Loc.t) :
    Loc.t =
  let start_offset =
    statement.reply_range.start.character
    - statement.query_range.start.character
  in
  let end_offset =
    statement.reply_range.end_.character - statement.query_range.end_.character
  in
  let loc_start =
    { loc.loc_start with pos_cnum = loc.loc_start.pos_cnum + start_offset }
  in
  let loc_end =
    { loc.loc_end with pos_cnum = loc.loc_end.pos_cnum + end_offset }
  in
  { loc with loc_start; loc_end }

(** Tries to find a statement we know how to handle on the line where the range
    starts. *)
let extract_statement (doc : Document.t) (ca_range : Range.t) :
    destructable_statement option =
  if ca_range.start.line <> ca_range.end_.line then None
  else
    let code = get_line doc ca_range in
    match get_statement_kind code ca_range with
    | None -> None
    | Some kind ->
      let query_range = get_query_range code kind ca_range in
      let reply_range = get_reply_range code kind query_range in
      Some { code; kind; query_range; reply_range }

(* Merlin often surrounds [line] (or part of it) with parentheses that we don't want. *)
let strip_parentheses =
  let regex =
    let open Re in
    seq [ str ")"; rep1 space; str "->"; rep1 space; char '_' ] |> compile
  in
  fun ~(kind : statement_kind) (line : string) ->
    (match kind with
    | MatchLine | MatchWithLine | Hole -> line
    | CaseLine -> Re.replace ~f:(fun _ -> " -> _") regex line)
    |> String.chop_prefix_if_exists ~prefix:"("
    |> String.chop_suffix_if_exists ~suffix:")"

let match_indent =
  let re = Re.str "\n| " |> Re.compile in
  fun ~(statement : destructable_statement) (new_code : string) ->
    let full_line = statement.code in
    let i =
      String.substr_index_exn full_line ~pattern:(String.strip full_line)
    in
    let indent = String.sub full_line ~pos:0 ~len:i in
    Re.replace ~f:(fun _ -> "\n" ^ indent ^ "| ") re new_code

(* TODO: If [ocamlformat_rpc] ever gets implemented, it would probably be worth
   re-thinking the post-processing that's happening here. *)
let format_merlin_reply =
  let start_of_case = Re.str " | " |> Re.compile in
  fun ~(statement : destructable_statement) (new_code : string) ->
    let lines = Re.split start_of_case new_code in
    let lines =
      match lines with
      | fst :: rst -> fst :: List.map ~f:String.strip rst
      | [] -> lines
    in
    match statement.kind with
    | MatchLine | MatchWithLine ->
      String.concat ~sep:"\n| " lines
      |> strip_parentheses ~kind:statement.kind
      |> match_indent ~statement
    | CaseLine ->
      List.map ~f:(strip_parentheses ~kind:statement.kind) lines
      |> String.concat ~sep:" -> _\n| "
      |> match_indent ~statement
    | Hole -> String.concat ~sep:" -> _\n| " lines |> match_indent ~statement

let code_action_of_case_analysis ~supportsJumpToNextHole doc uri (loc, newText)
    =
  let range : Range.t = Range.of_loc loc in
  let textedit : TextEdit.t = { range; newText } in
  let edit : WorkspaceEdit.t =
    let version = Document.version doc in
    let textDocument =
      OptionalVersionedTextDocumentIdentifier.create ~uri ~version ()
    in
    let edit =
      TextDocumentEdit.create ~textDocument ~edits:[ `TextEdit textedit ]
    in
    WorkspaceEdit.create ~documentChanges:[ `TextDocumentEdit edit ] ()
  in
  let title = String.capitalize action_kind in
  let command =
    if supportsJumpToNextHole then
      Some
        (Client.Custom_commands.next_hole
           ~in_range:(Range.resize_for_edit textedit)
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

let dispatch_destruct (merlin : Document.Merlin.t) (range : Range.t) =
  let command =
    let start = Position.logical range.start in
    let finish = Position.logical range.end_ in
    Query_protocol.Case_analysis (start, finish)
  in
  Document.Merlin.dispatch ~name:"destruct" merlin command

let code_action (state : State.t) (doc : Document.t)
    (params : CodeActionParams.t) =
  let uri = params.textDocument.uri in
  match Document.kind doc with
  | `Other -> Fiber.return None
  | `Merlin merlin -> (
    match (Document.Merlin.kind merlin, extract_statement doc params.range) with
    | Intf, _ | _, None -> Fiber.return None
    | Impl, Some statement -> (
      let+ res = dispatch_destruct merlin statement.query_range in
      match res with
      | Ok (loc, newText) ->
        let loc = adjust_reply_location ~statement loc in
        let newText = format_merlin_reply ~statement newText in
        let supportsJumpToNextHole =
          State.experimental_client_capabilities state
          |> Client.Experimental_capabilities.supportsJumpToNextHole
        in
        Some
          (code_action_of_case_analysis
             ~supportsJumpToNextHole
             doc
             uri
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

let t state = { Code_action.kind; run = `Non_batchable (code_action state) }
