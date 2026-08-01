open Base
open Base_quickcheck
open Lsp.Types
module Semantic_tokens = Lsp.Semantic_tokens

let select selector ~choices = Int.rem (selector land Int.max_value) choices
let check label condition = if not condition then failwith label

let check_string_list label ~expected ~actual =
  if not (List.equal String.equal expected actual)
  then
    failwith
      (Printf.sprintf
         "%s: expected %s, got %s"
         label
         (Sexp.to_string_hum ([%sexp_of: string list] expected))
         (Sexp.to_string_hum ([%sexp_of: string list] actual)))
;;

let check_int_option label ~context ~expected ~actual =
  if not (Option.equal Int.equal expected actual)
  then
    failwith
      (Printf.sprintf
         "%s (%s): expected %s, got %s"
         label
         context
         (Sexp.to_string_hum ([%sexp_of: int option] expected))
         (Sexp.to_string_hum ([%sexp_of: int option] actual)))
;;

let check_int label ~context ~expected ~actual =
  if expected <> actual
  then
    failwith (Printf.sprintf "%s (%s): expected %d, got %d" label context expected actual)
;;

let value_index values value =
  List.find_mapi values ~f:(fun index candidate ->
    Option.some_if (String.equal value candidate) index)
;;

let supported_values server_values client_values =
  List.filter server_values ~f:(fun server_value ->
    List.mem client_values server_value ~equal:String.equal)
;;

let modifier_names_of_bits bits modifiers =
  List.filter_mapi modifiers ~f:(fun index modifier ->
    Option.some_if (bits land (1 lsl index) <> 0) modifier)
;;

let bits_of_modifier_names names modifiers =
  List.fold_left names ~init:0 ~f:(fun bits name ->
    match value_index modifiers name with
    | None -> bits
    | Some index -> bits lor (1 lsl index))
;;

let legend token_types token_modifiers =
  Semantic_tokens.Legend.create ~token_types ~token_modifiers
;;

module Token_case = struct
  type token =
    { line_delta_selector : int
    ; character_selector : int
    ; length_selector : int
    ; token_type_selector : int
    ; token_modifiers_selector : int
    }
  [@@deriving quickcheck, sexp_of]

  type t = token list [@@deriving quickcheck, sexp_of]
end

type absolute_token =
  { line : int
  ; character : int
  ; length : int
  ; token_type : int
  ; token_modifiers : int
  }

let absolute_tokens seeds =
  let _, _, tokens =
    List.fold
      seeds
      ~init:(0, 0, [])
      ~f:
        (fun
          (previous_line, previous_character, tokens)
          { Token_case.line_delta_selector
          ; character_selector
          ; length_selector
          ; token_type_selector
          ; token_modifiers_selector
          }
        ->
        let line_delta = select line_delta_selector ~choices:4 in
        let line = previous_line + line_delta in
        let character =
          if line_delta = 0
          then previous_character + select character_selector ~choices:8
          else select character_selector ~choices:24
        in
        let length = select length_selector ~choices:12 + 1 in
        let token_type = select token_type_selector ~choices:8 in
        let token_modifiers = select token_modifiers_selector ~choices:16 in
        ( line
        , character
        , { line; character; length; token_type; token_modifiers } :: tokens ))
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
      let token_type = encoded.(index + 3) in
      let token_modifiers = encoded.(index + 4) in
      check "negative delta line" (delta_line >= 0);
      check "negative delta start" (delta_start >= 0);
      check "nonpositive token length" (length > 0);
      let line = previous_line + delta_line in
      let character =
        if delta_line = 0 then previous_character + delta_start else delta_start
      in
      loop
        (index + 5)
        line
        character
        ({ line; character; length; token_type; token_modifiers } :: tokens))
  in
  loop 0 0 0 []
;;

let equal_token left right =
  left.line = right.line
  && left.character = right.character
  && left.length = right.length
  && left.token_type = right.token_type
  && left.token_modifiers = right.token_modifiers
;;

let sort_tokens tokens =
  List.stable_sort tokens ~compare:(fun left right ->
    match Int.compare left.line right.line with
    | 0 -> Int.compare left.character right.character
    | c -> c)
;;

let to_wire_token { line; character; length; token_type; token_modifiers } =
  { Semantic_tokens.Token.start = Position.create ~line ~character
  ; length
  ; token_type
  ; token_modifiers
  }
;;

let check_token_case seeds =
  let expected = absolute_tokens seeds |> sort_tokens in
  let input = List.map expected ~f:to_wire_token in
  let actual = Semantic_tokens.encode input |> decode in
  check "semantic token round trip" (List.equal equal_token expected actual)
;;

let%test_unit "semantic token encoding sorts out-of-order tokens" =
  let tokens =
    [ { line = 2; character = 0; length = 1; token_type = 1; token_modifiers = 0 }
    ; { line = 1; character = 4; length = 2; token_type = 2; token_modifiers = 1 }
    ; { line = 1; character = 0; length = 3; token_type = 3; token_modifiers = 2 }
    ]
  in
  let expected = sort_tokens tokens in
  let actual = Semantic_tokens.encode (List.map tokens ~f:to_wire_token) |> decode in
  check "semantic token out-of-order sort" (List.equal equal_token expected actual)
;;

module Capability_case = struct
  type t =
    { server_token_types : string list
    ; server_token_modifiers : string list
    ; client_token_types : string list
    ; client_token_modifiers : string list
    ; token_type_index : int
    ; token_modifier_bits : int
    }
  [@@deriving sexp_of]

  let unknown = "unknown"

  let gen_names ~prefix ~count =
    Generator.return
      (List.init count ~f:(fun index -> Printf.sprintf "%s%d" prefix index))
  ;;

  let gen_client_values server_values =
    Generator.bind
      (Generator.list_with_length Generator.bool ~length:(List.length server_values))
      ~f:(fun includes ->
        let supported =
          List.filter_mapi server_values ~f:(fun index server_value ->
            Option.some_if (List.nth_exn includes index) server_value)
        in
        Generator.bind (Generator.int_inclusive 0 2) ~f:(fun unknown_count ->
          let unknowns = List.init unknown_count ~f:(fun _ -> unknown) in
          Generator.bind
            (Generator.list_with_length Generator.bool ~length:(List.length supported))
            ~f:(fun duplicate_flags ->
              let duplicates =
                List.filter_mapi supported ~f:(fun index value ->
                  Option.some_if (List.nth_exn duplicate_flags index) value)
              in
              Generator.list_permutations (supported @ unknowns @ duplicates))))
  ;;

  let quickcheck_generator =
    Generator.bind (Generator.int_inclusive 1 6) ~f:(fun type_count ->
      Generator.bind (Generator.int_inclusive 1 6) ~f:(fun modifier_count ->
        Generator.bind
          (gen_names ~prefix:"type" ~count:type_count)
          ~f:(fun server_token_types ->
            Generator.bind
              (gen_names ~prefix:"mod" ~count:modifier_count)
              ~f:(fun server_token_modifiers ->
                Generator.bind
                  (gen_client_values server_token_types)
                  ~f:(fun client_token_types ->
                    Generator.bind
                      (gen_client_values server_token_modifiers)
                      ~f:(fun client_token_modifiers ->
                        Generator.bind
                          (Generator.int_inclusive 0 (type_count - 1))
                          ~f:(fun token_type_index ->
                            Generator.map
                              (Generator.int_inclusive 0 ((1 lsl modifier_count) - 1))
                              ~f:(fun token_modifier_bits ->
                                { server_token_types
                                ; server_token_modifiers
                                ; client_token_types
                                ; client_token_modifiers
                                ; token_type_index
                                ; token_modifier_bits
                                }))))))))
  ;;

  let quickcheck_observer =
    Observer.unmap
      [%quickcheck.observer:
        string list * string list * string list * string list * int * int]
      ~f:
        (fun
          { server_token_types
          ; server_token_modifiers
          ; client_token_types
          ; client_token_modifiers
          ; token_type_index
          ; token_modifier_bits
          }
        ->
        ( server_token_types
        , server_token_modifiers
        , client_token_types
        , client_token_modifiers
        , token_type_index
        , token_modifier_bits ))
  ;;

  let quickcheck_shrinker =
    Shrinker.map
      [%quickcheck.shrinker:
        string list * string list * string list * string list * int * int]
      ~f:
        (fun
          ( server_token_types
          , server_token_modifiers
          , client_token_types
          , client_token_modifiers
          , token_type_index
          , token_modifier_bits ) ->
        { server_token_types
        ; server_token_modifiers
        ; client_token_types
        ; client_token_modifiers
        ; token_type_index
        ; token_modifier_bits
        })
      ~f_inverse:
        (fun
          { server_token_types
          ; server_token_modifiers
          ; client_token_types
          ; client_token_modifiers
          ; token_type_index
          ; token_modifier_bits
          } ->
        ( server_token_types
        , server_token_modifiers
        , client_token_types
        , client_token_modifiers
        , token_type_index
        , token_modifier_bits ))
  ;;
end

let check_capability_case
      { Capability_case.server_token_types
      ; server_token_modifiers
      ; client_token_types
      ; client_token_modifiers
      ; token_type_index
      ; token_modifier_bits
      }
  =
  let server = legend server_token_types server_token_modifiers in
  let client = legend client_token_types client_token_modifiers in
  let encoding = Semantic_tokens.Encoding.negotiate ~server ~client in
  let negotiated = Semantic_tokens.Encoding.legend encoding in
  let expected_token_types = supported_values server_token_types client_token_types in
  let expected_token_modifiers =
    supported_values server_token_modifiers client_token_modifiers
  in
  check_string_list
    "negotiated semantic token types"
    ~expected:expected_token_types
    ~actual:(Semantic_tokens.Legend.token_types negotiated);
  check_string_list
    "negotiated semantic token modifiers"
    ~expected:expected_token_modifiers
    ~actual:(Semantic_tokens.Legend.token_modifiers negotiated);
  let server_token_type = List.nth_exn server_token_types token_type_index in
  let expected_type_index =
    value_index (Semantic_tokens.Legend.token_types negotiated) server_token_type
  in
  let actual_type_index =
    Semantic_tokens.Encoding.token_type_index encoding ~server_index:token_type_index
  in
  check_int_option
    "semantic token type index remapping"
    ~context:server_token_type
    ~expected:expected_type_index
    ~actual:actual_type_index;
  let expected_modifier_names =
    modifier_names_of_bits token_modifier_bits server_token_modifiers
    |> List.filter ~f:(fun name ->
      List.mem
        (Semantic_tokens.Legend.token_modifiers negotiated)
        name
        ~equal:String.equal)
  in
  let expected_modifier_bits =
    bits_of_modifier_names
      expected_modifier_names
      (Semantic_tokens.Legend.token_modifiers negotiated)
  in
  let actual_modifier_bits =
    Semantic_tokens.Encoding.token_modifiers_bitset
      encoding
      ~server_bitset:token_modifier_bits
  in
  check_int
    "semantic token modifier bit remapping"
    ~context:(Sexp.to_string_hum ([%sexp_of: string list] expected_modifier_names))
    ~expected:expected_modifier_bits
    ~actual:actual_modifier_bits;
  (* Encode through the public API when the type is supported. *)
  match expected_type_index with
  | None -> ()
  | Some negotiated_type ->
    let encoded =
      Semantic_tokens.encode
        [ { Semantic_tokens.Token.start = Position.create ~line:1 ~character:2
          ; length = 3
          ; token_type = negotiated_type
          ; token_modifiers = actual_modifier_bits
          }
        ]
    in
    check_int
      "encoded token count"
      ~context:server_token_type
      ~expected:5
      ~actual:(Array.length encoded);
    check_int
      "encoded delta line"
      ~context:server_token_type
      ~expected:1
      ~actual:encoded.(0);
    check_int
      "encoded delta start"
      ~context:server_token_type
      ~expected:2
      ~actual:encoded.(1);
    check_int "encoded length" ~context:server_token_type ~expected:3 ~actual:encoded.(2);
    check_int
      "encoded token type"
      ~context:server_token_type
      ~expected:negotiated_type
      ~actual:encoded.(3);
    check_int
      "encoded token modifiers"
      ~context:server_token_type
      ~expected:actual_modifier_bits
      ~actual:encoded.(4)
;;

let capability_regressions =
  [ { Capability_case.server_token_types = [ "variable"; "number"; "function" ]
    ; server_token_modifiers = [ "declaration"; "definition"; "readonly" ]
    ; client_token_types = []
    ; client_token_modifiers = []
    ; token_type_index = 0
    ; token_modifier_bits = 0
    }
  ; { server_token_types = [ "variable"; "number"; "function" ]
    ; server_token_modifiers = [ "declaration"; "definition"; "readonly" ]
    ; client_token_types = [ "unknown" ]
    ; client_token_modifiers = [ "unknown" ]
    ; token_type_index = 1
    ; token_modifier_bits = 1 lsl 1
    }
  ; { server_token_types = [ "variable"; "number"; "function" ]
    ; server_token_modifiers = [ "declaration"; "definition"; "readonly" ]
    ; client_token_types = [ "number"; "variable"; "nope"; "variable" ]
    ; client_token_modifiers = [ "definition"; "definition"; "unknown" ]
    ; token_type_index = 0
    ; token_modifier_bits = 1 lsl 1
    }
  ; { server_token_types = [ "variable"; "number"; "function" ]
    ; server_token_modifiers = [ "declaration"; "definition"; "readonly" ]
    ; client_token_types = [ "function"; "number"; "variable" ]
    ; client_token_modifiers = [ "readonly"; "definition"; "declaration" ]
    ; token_type_index = 2
    ; token_modifier_bits = (1 lsl 3) - 1
    }
  ]
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

let%test_unit "semantic token capability negotiation preserves supported values" =
  Test.run_exn
    (module Capability_case)
    ~examples:capability_regressions
    ~f:check_capability_case
;;

let%test_unit "semantic token deltas reconstruct the new data" =
  Test.run_exn (module Diff_case) ~examples:diff_regressions ~f:check_diff_case
;;
