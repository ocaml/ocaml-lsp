open Base
open Base_quickcheck
open Lsp.Types
module Semantic_tokens = Ocaml_lsp_server.Testing.Semantic_tokens

let select selector ~choices = Int.rem (selector land Stdlib.max_int) choices
let check label condition = if not condition then failwith label

module Token_case = struct
  type token =
    { line_delta_selector : int
    ; character_selector : int
    ; length_selector : int
    }
  [@@deriving quickcheck, sexp_of]

  type t = token list [@@deriving quickcheck, sexp_of]
end

type token =
  { line : int
  ; character : int
  ; length : int
  }

let absolute_tokens seeds =
  let _, _, tokens =
    List.fold
      seeds
      ~init:(0, 0, [])
      ~f:
        (fun
          (previous_line, previous_character, tokens)
          { Token_case.line_delta_selector; character_selector; length_selector }
        ->
        let line_delta = select line_delta_selector ~choices:4 in
        let line = previous_line + line_delta in
        let character =
          if line_delta = 0
          then previous_character + select character_selector ~choices:8
          else select character_selector ~choices:24
        in
        let length = select length_selector ~choices:12 + 1 in
        line, character, { line; character; length } :: tokens)
  in
  List.rev tokens
;;

let decode encoded =
  check "semantic token array length" (Int.rem (Array.length encoded) 5 = 0);
  let rec loop index previous_line previous_character tokens =
    if index = Array.length encoded
    then List.rev tokens
    else (
      let delta_line = encoded.(index) in
      let delta_start = encoded.(index + 1) in
      let length = encoded.(index + 2) in
      check "negative delta line" (delta_line >= 0);
      check "negative delta start" (delta_start >= 0);
      check "nonpositive token length" (length > 0);
      check "wrong token type" (encoded.(index + 3) = Semantic_tokens.token_type_index);
      check
        "wrong token modifiers"
        (encoded.(index + 4) = Semantic_tokens.token_modifiers_bitset);
      let line = previous_line + delta_line in
      let character =
        if delta_line = 0 then previous_character + delta_start else delta_start
      in
      loop (index + 5) line character ({ line; character; length } :: tokens))
  in
  loop 0 0 0 []
;;

let equal_token left right =
  left.line = right.line && left.character = right.character && left.length = right.length
;;

let check_token_case seeds =
  let expected = absolute_tokens seeds in
  let input =
    List.map expected ~f:(fun { line; character; length } ->
      Position.create ~line ~character, length)
  in
  let actual = Semantic_tokens.encode input |> decode in
  check "semantic token round trip" (List.equal equal_token expected actual)
;;

module Diff_case = struct
  type t =
    { old : int list
    ; new_ : int list
    }
  [@@deriving quickcheck, sexp_of]
end

let apply_edit
      source
      ({ SemanticTokensEdit.start; deleteCount; data } : SemanticTokensEdit.t)
  =
  check "negative semantic token edit start" (start >= 0);
  check "negative semantic token delete count" (deleteCount >= 0);
  check "semantic token edit start out of bounds" (start <= Array.length source);
  check
    "semantic token edit deletion out of bounds"
    (deleteCount <= Array.length source - start);
  let replacement = Option.value data ~default:[||] in
  Array.concat
    [ Array.sub source ~pos:0 ~len:start
    ; replacement
    ; Array.sub
        source
        ~pos:(start + deleteCount)
        ~len:(Array.length source - start - deleteCount)
    ]
;;

let check_diff_case { Diff_case.old; new_ } =
  let old = Array.of_list old in
  let expected = Array.of_list new_ in
  let edits = Semantic_tokens.find_diff ~old ~new_:expected in
  check "semantic token diff produced multiple edits" (List.length edits <= 1);
  check
    "equal semantic token arrays produced an edit"
    (Bool.equal (List.is_empty edits) (Array.equal Int.equal old expected));
  let actual = List.fold edits ~init:old ~f:apply_edit in
  check "semantic token diff round trip" (Array.equal Int.equal expected actual)
;;

let diff_regressions =
  [ { Diff_case.old = []; new_ = [] }
  ; { old = []; new_ = [ 1; 2 ] }
  ; { old = [ 1; 2 ]; new_ = [] }
  ; { old = [ 1; 2 ]; new_ = [ 1; 2; 3 ] }
  ; { old = [ 1; 2; 3 ]; new_ = [ 1; 2 ] }
  ; { old = [ 1; 2; 1 ]; new_ = [ 1; 3; 1 ] }
  ; { old = [ 1; 1 ]; new_ = [ 1 ] }
  ; { old = [ 1 ]; new_ = [ 1; 1 ] }
  ]
;;

let%test_unit "semantic token encoding round trips" =
  Test.run_exn (module Token_case) ~f:check_token_case
;;

let%test_unit "semantic token deltas reconstruct the new data" =
  Test.run_exn (module Diff_case) ~examples:diff_regressions ~f:check_diff_case
;;
