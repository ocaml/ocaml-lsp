open Base
open Base_quickcheck
open Lsp.Types
module Semantic_tokens = Ocaml_lsp_server.Testing.Semantic_tokens

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

module Capability_case = struct
  type t =
    { token_types : string list
    ; token_modifiers : string list
    ; token_type_index : int
    ; token_modifier_bits : int
    }
  [@@deriving sexp_of]

  let unknown = "unknown"

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
    let token_type_count = List.length Semantic_tokens.server_token_types in
    let token_modifier_count = List.length Semantic_tokens.server_token_modifiers in
    Generator.bind
      (gen_client_values Semantic_tokens.server_token_types)
      ~f:(fun token_types ->
        Generator.bind
          (gen_client_values Semantic_tokens.server_token_modifiers)
          ~f:(fun token_modifiers ->
            Generator.bind
              (Generator.int_inclusive 0 (token_type_count - 1))
              ~f:(fun token_type_index ->
                Generator.map
                  (Generator.int_inclusive 0 ((1 lsl token_modifier_count) - 1))
                  ~f:(fun token_modifier_bits ->
                    { token_types
                    ; token_modifiers
                    ; token_type_index
                    ; token_modifier_bits
                    }))))
  ;;

  let quickcheck_observer =
    Observer.unmap
      [%quickcheck.observer: string list * string list * int * int]
      ~f:(fun { token_types; token_modifiers; token_type_index; token_modifier_bits } ->
        token_types, token_modifiers, token_type_index, token_modifier_bits)
  ;;

  let quickcheck_shrinker =
    Shrinker.map
      [%quickcheck.shrinker: string list * string list * int * int]
      ~f:(fun (token_types, token_modifiers, token_type_index, token_modifier_bits) ->
        { token_types; token_modifiers; token_type_index; token_modifier_bits })
      ~f_inverse:
        (fun
          { token_types; token_modifiers; token_type_index; token_modifier_bits } ->
        token_types, token_modifiers, token_type_index, token_modifier_bits)
  ;;
end

let sample_position = Position.create ~line:0 ~character:0

let check_capability_case
      { Capability_case.token_types
      ; token_modifiers
      ; token_type_index
      ; token_modifier_bits
      }
  =
  let config = Semantic_tokens.create_config ~token_types ~token_modifiers in
  let legend = Semantic_tokens.legend config in
  let expected_token_types =
    supported_values Semantic_tokens.server_token_types token_types
  in
  let expected_token_modifiers =
    supported_values Semantic_tokens.server_token_modifiers token_modifiers
  in
  check_string_list
    "negotiated semantic token types"
    ~expected:expected_token_types
    ~actual:legend.tokenTypes;
  check_string_list
    "negotiated semantic token modifiers"
    ~expected:expected_token_modifiers
    ~actual:legend.tokenModifiers;
  let server_token_type =
    List.nth_exn Semantic_tokens.server_token_types token_type_index
  in
  let expected_type_index = value_index legend.tokenTypes server_token_type in
  let encoded =
    Semantic_tokens.encode
      ~config
      ~token_type_index
      ~token_modifiers:token_modifier_bits
      [ sample_position, 1 ]
  in
  let actual_type_index = if Array.length encoded = 0 then None else Some encoded.(3) in
  check_int_option
    "semantic token type index remapping"
    ~context:server_token_type
    ~expected:expected_type_index
    ~actual:actual_type_index;
  let expected_modifier_names =
    modifier_names_of_bits token_modifier_bits Semantic_tokens.server_token_modifiers
    |> List.filter ~f:(List.mem legend.tokenModifiers ~equal:String.equal)
  in
  let expected_modifier_bits =
    bits_of_modifier_names expected_modifier_names legend.tokenModifiers
  in
  let actual_modifier_bits = if Array.length encoded = 0 then 0 else encoded.(4) in
  match expected_type_index with
  | None ->
    check_int
      "unsupported token type is dropped"
      ~context:server_token_type
      ~expected:0
      ~actual:(Array.length encoded)
  | Some _ ->
    check_int
      "semantic token modifier bit remapping"
      ~context:(Sexp.to_string_hum ([%sexp_of: string list] expected_modifier_names))
      ~expected:expected_modifier_bits
      ~actual:actual_modifier_bits
;;

let capability_regressions =
  let all_types = Semantic_tokens.server_token_types in
  let all_modifiers = Semantic_tokens.server_token_modifiers in
  let variable_index = Option.value_exn (value_index all_types "variable") in
  let number_index = Option.value_exn (value_index all_types "number") in
  let definition_bit = 1 lsl Option.value_exn (value_index all_modifiers "definition") in
  [ { Capability_case.token_types = []
    ; token_modifiers = []
    ; token_type_index = variable_index
    ; token_modifier_bits = 0
    }
  ; { token_types = [ "unknown" ]
    ; token_modifiers = [ "unknown" ]
    ; token_type_index = variable_index
    ; token_modifier_bits = definition_bit
    }
  ; { token_types = [ "variable"; "number"; "nope" ]
    ; token_modifiers = [ "definition"; "definition"; "unknown" ]
    ; token_type_index = number_index
    ; token_modifier_bits = definition_bit
    }
  ; { token_types = [ "number"; "variable" ]
    ; token_modifiers = [ "readonly"; "definition" ]
    ; token_type_index = variable_index
    ; token_modifier_bits = definition_bit
    }
  ; { token_types = all_types
    ; token_modifiers = all_modifiers
    ; token_type_index = variable_index
    ; token_modifier_bits = (1 lsl List.length all_modifiers) - 1
    }
  ]
;;

let%test_unit "semantic token capability negotiation preserves supported values" =
  Test.run_exn
    (module Capability_case)
    ~examples:capability_regressions
    ~f:check_capability_case
;;
