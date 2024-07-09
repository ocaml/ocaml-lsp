open Test.Import

type token =
  { delta_line : int
  ; delta_char : int
  ; len : int
  ; type_ : int
  ; mods : int
  }

let tokens encoded_tokens =
  Array.init
    (Array.length encoded_tokens / 5)
    ~f:(fun i ->
      let ix = i * 5 in
      { delta_line = encoded_tokens.(ix)
      ; delta_char = encoded_tokens.(ix + 1)
      ; len = encoded_tokens.(ix + 2)
      ; type_ = encoded_tokens.(ix + 3)
      ; mods = encoded_tokens.(ix + 4)
      })
;;

let modifiers ~(legend : string array) (encoded_mods : int) =
  let rec loop encoded_mods i acc =
    if encoded_mods = 0
    then acc
    else (
      let k = Stdlib.Int.logand encoded_mods 1 in
      let new_val = Stdlib.Int.shift_right encoded_mods 1 in
      if k = 0 then loop new_val (i + 1) acc else loop new_val (i + 1) (legend.(k) :: acc))
  in
  loop encoded_mods 0 [] |> List.rev
;;

let annotate_src_with_tokens
  ~(legend : SemanticTokensLegend.t)
  ~(encoded_tokens : int array)
  ~(annot_mods : bool)
  (src : string)
  : string
  =
  let token_types = legend.SemanticTokensLegend.tokenTypes |> Array.of_list in
  let token_mods = legend.SemanticTokensLegend.tokenModifiers |> Array.of_list in
  let src_ix = ref 0 in
  let tokens = Array.Iter.create (tokens encoded_tokens) in
  let token = ref @@ Array.Iter.next_exn tokens in
  let token_id = ref 0 in
  let line = ref !token.delta_line in
  let character = ref !token.delta_char in
  let src_len = String.length src in
  let b = Buffer.create src_len in
  while !src_ix < src_len do
    if !line = 0 && !character = 0
    then (
      Printf.bprintf
        b
        "<%s%s-%d>"
        token_types.(!token.type_)
        (if annot_mods
         then "|" ^ String.concat ~sep:"," (modifiers ~legend:token_mods !token.mods)
         else "")
        !token_id;
      Buffer.add_substring b src !src_ix !token.len;
      src_ix := !src_ix + !token.len;
      Printf.bprintf b "</%d>" !token_id;
      match Array.Iter.next tokens with
      | None ->
        (* copy the rest of src *)
        Buffer.add_substring b src !src_ix (src_len - !src_ix);
        src_ix := src_len
      | Some next_token ->
        incr token_id;
        character := next_token.delta_char - !token.len;
        token := next_token;
        line := !token.delta_line)
    else (
      let ch = src.[!src_ix] in
      (match ch with
       | '\n' ->
         decr line;
         character := !token.delta_char
       | _ -> decr character);
      Buffer.add_char b ch;
      incr src_ix)
  done;
  Buffer.to_bytes b |> Bytes.to_string
;;

(* for tests below *)
let legend = SemanticTokensLegend.create ~tokenModifiers:[] ~tokenTypes:[ "var"; "mod" ]

let%expect_test "annotate single-line src" =
  annotate_src_with_tokens
    ~legend
    ~encoded_tokens:[| 0; 4; 3; 0; 0 |]
    ~annot_mods:false
    {|let foo = bar|}
  |> print_endline;
  [%expect {| let <var-0>foo</0> = bar |}]
;;

let%expect_test "annotate multi-line src" =
  annotate_src_with_tokens
    ~legend
    ~encoded_tokens:[| 0; 4; 3; 0; 0; 0; 6; 3; 0; 0; 1; 4; 3; 0; 0; 0; 6; 3; 1; 0 |]
    ~annot_mods:false
    {|let foo = bar
let jar = dar|}
  |> print_endline;
  [%expect
    {|
    let <var-0>foo</0> = <var-1>bar</1>
    let <var-2>jar</2> = <mod-3>dar</3> |}]
;;
