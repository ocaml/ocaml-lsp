open Import
open Types

module Legend = struct
  type t =
    { token_types : string list
    ; token_modifiers : string list
    }

  let create ~token_types ~token_modifiers = { token_types; token_modifiers }
  let token_types t = t.token_types
  let token_modifiers t = t.token_modifiers

  let to_types t =
    SemanticTokensLegend.create
      ~tokenTypes:t.token_types
      ~tokenModifiers:t.token_modifiers
  ;;
end

module Encoding = struct
  type t =
    { legend : Legend.t
    ; token_type_indices : int option array
    ; token_modifier_bits : int option array
    }

  let mem value client = List.exists client ~f:(String.equal value)

  let supported_values ~server ~client =
    List.filter server ~f:(fun value -> mem value client)
  ;;

  let index_of values value =
    let rec loop index = function
      | [] -> None
      | candidate :: rest ->
        if String.equal value candidate then Some index else loop (index + 1) rest
    in
    loop 0 values
  ;;

  let negotiate_token_type_indices ~server_token_types ~token_types =
    server_token_types
    |> Array.of_list
    |> Array.map ~f:(fun server_token_type -> index_of token_types server_token_type)
  ;;

  let negotiate_token_modifier_bits ~server_token_modifiers ~token_modifiers =
    server_token_modifiers
    |> Array.of_list
    |> Array.map ~f:(fun server_modifier ->
      match index_of token_modifiers server_modifier with
      | None -> None
      | Some index -> Some (1 lsl index))
  ;;

  let negotiate ~server ~client =
    let token_types =
      supported_values
        ~server:(Legend.token_types server)
        ~client:(Legend.token_types client)
    in
    let token_modifiers =
      supported_values
        ~server:(Legend.token_modifiers server)
        ~client:(Legend.token_modifiers client)
    in
    let legend = Legend.create ~token_types ~token_modifiers in
    let token_type_indices =
      negotiate_token_type_indices
        ~server_token_types:(Legend.token_types server)
        ~token_types
    in
    let token_modifier_bits =
      negotiate_token_modifier_bits
        ~server_token_modifiers:(Legend.token_modifiers server)
        ~token_modifiers
    in
    { legend; token_type_indices; token_modifier_bits }
  ;;

  let legend t = t.legend

  let token_type_index t ~server_index =
    if server_index < 0 || server_index >= Array.length t.token_type_indices
    then None
    else t.token_type_indices.(server_index)
  ;;

  let token_modifiers_bitset t ~server_bitset =
    let rec loop server_index encoded result =
      if Int.equal encoded 0
      then result
      else (
        let result =
          if Int.equal (encoded land 1) 0
          then result
          else if server_index < 0 || server_index >= Array.length t.token_modifier_bits
          then result
          else (
            match t.token_modifier_bits.(server_index) with
            | None -> result
            | Some client_bit -> result lor client_bit)
        in
        loop (server_index + 1) (encoded lsr 1) result)
    in
    loop 0 server_bitset 0
  ;;
end

module Token = struct
  type t =
    { start : Position.t
    ; length : int
    ; token_type : int
    ; token_modifiers : int
    }
end

let set_token
      arr
      ~delta_line_index
      ~delta_line
      ~delta_start
      ~length
      ~token_type
      ~token_modifiers
  =
  arr.(delta_line_index) <- delta_line;
  arr.(delta_line_index + 1) <- delta_start;
  arr.(delta_line_index + 2) <- length;
  arr.(delta_line_index + 3) <- token_type;
  arr.(delta_line_index + 4) <- token_modifiers
;;

let compare_position (left : Position.t) (right : Position.t) =
  match Int.compare left.line right.line with
  | 0 -> Int.compare left.character right.character
  | c -> c
;;

let encode (tokens : Token.t list) =
  (* Stable so equal-position tokens keep caller order. *)
  let tokens =
    List.stable_sort tokens ~cmp:(fun (left : Token.t) right ->
      compare_position left.start right.start)
  in
  let data = Array.make (List.length tokens * 5) 0 in
  let rec loop index previous_line previous_character = function
    | [] -> ()
    | (token : Token.t) :: rest ->
      let delta_line = token.start.line - previous_line in
      let delta_start =
        if Int.equal delta_line 0
        then token.start.character - previous_character
        else token.start.character
      in
      set_token
        data
        ~delta_line_index:(index * 5)
        ~delta_line
        ~delta_start
        ~length:token.length
        ~token_type:token.token_type
        ~token_modifiers:token.token_modifiers;
      loop (index + 1) token.start.line token.start.character rest
  in
  loop 0 0 0 tokens;
  data
;;

let common_prefix_len old new_ =
  let len = min (Array.length old) (Array.length new_) in
  let rec loop i = if i = len || old.(i) <> new_.(i) then i else loop (i + 1) in
  loop 0
;;

(* [find_diff] finds common prefix and common suffix and reports the rest as
   array difference. This is not ideal but good enough. The idea comes from the
   Rust Analyzer implementation of this function. *)
let find_diff ~(old : int array) ~(new_ : int array) : SemanticTokensEdit.t list =
  let old_len = Array.length old in
  let new_len = Array.length new_ in
  let left_offset = common_prefix_len old new_ in
  if left_offset = old_len
  then
    if left_offset = new_len
    then (* [old] and [new_] are simply equal *) []
    else
      (* [old] is prefix of [new_] *)
      [ SemanticTokensEdit.create
          ~start:left_offset
          ~deleteCount:0
          ~data:(Array.sub new_ ~pos:left_offset ~len:(new_len - left_offset))
          ()
      ]
  else if left_offset = new_len
  then
    (* [new_] is prefix of [old] *)
    [ SemanticTokensEdit.create ~start:left_offset ~deleteCount:(old_len - left_offset) ()
    ]
  else (
    let common_suffix_len =
      let old_noncommon = Array_view.make old ~pos:left_offset in
      let new_noncommon = Array_view.make new_ ~pos:left_offset in
      Array_view.common_suffix_len old_noncommon new_noncommon
    in
    let deleteCount =
      let right_offset_old = old_len - common_suffix_len in
      right_offset_old - left_offset
    in
    let data =
      let right_offset_new = new_len - common_suffix_len in
      Array.sub new_ ~pos:left_offset ~len:(right_offset_new - left_offset)
    in
    [ SemanticTokensEdit.create ~start:left_offset ~deleteCount ~data () ])
;;
