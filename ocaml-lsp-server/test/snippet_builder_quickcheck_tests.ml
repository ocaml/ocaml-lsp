open Base
open Base_quickcheck
module Builder = Ocaml_lsp_server.Testing.Snippet_builder
module Snippet = Lsp.Snippet

type fragment =
  | Variable
  | String_dollar
  | String_close_brace
  | String_backslash
  | Expression_hole
  | Hole_identifier
  | String_hole
  | Character_hole
  | Comment_hole
  | Nested_comment_hole
  | Quoted_string_hole
  | Function_with_wildcard
  | Match_with_wildcards
  | Tuple_holes
[@@deriving quickcheck, sexp_of]

module Source_case = struct
  type t = fragment list [@@deriving quickcheck, sexp_of]
end

let fragment = function
  | Variable -> "x"
  | String_dollar -> "\"$\""
  | String_close_brace -> "\"}\""
  | String_backslash -> "\"\\\\\""
  | Expression_hole -> "_"
  | Hole_identifier -> "_value"
  | String_hole -> "\"_\""
  | Character_hole -> "'_'"
  | Comment_hole -> "((* _ *) x)"
  | Nested_comment_hole -> "((* outer (* _ *) *) x)"
  | Quoted_string_hole -> "{tag|_ $}\\|tag}"
  | Function_with_wildcard -> "(fun _ -> _)"
  | Match_with_wildcards -> "(match _ with | Some _ -> _ | None -> _)"
  | Tuple_holes -> "(_, _)"
;;

let placeholder_count = function
  | Expression_hole -> 1
  | Function_with_wildcard -> 1
  | Match_with_wildcards -> 3
  | Tuple_holes -> 2
  | Variable
  | String_dollar
  | String_close_brace
  | String_backslash
  | Hole_identifier
  | String_hole
  | Character_hole
  | Comment_hole
  | Nested_comment_hole
  | Quoted_string_hole -> 0
;;

let source fragments =
  match fragments with
  | [] -> "()"
  | _ -> "(" ^ (List.map fragments ~f:fragment |> String.concat ~sep:", ") ^ ")"
;;

let defaults snippet =
  let length = String.length snippet in
  let buffer = Buffer.create length in
  let rec digits index =
    if index < length && Char.is_digit snippet.[index] then digits (index + 1) else index
  in
  let rec loop index =
    if index = length
    then Buffer.contents buffer
    else (
      match snippet.[index] with
      | '\\' when index + 1 < length ->
        Buffer.add_char buffer snippet.[index + 1];
        loop (index + 2)
      | '$' when index + 1 < length && Char.equal snippet.[index + 1] '0' ->
        loop (index + 2)
      | '$' when index + 1 < length && Char.equal snippet.[index + 1] '{' ->
        let after_index = digits (index + 2) in
        if
          after_index + 2 >= length
          || (not (Char.equal snippet.[after_index] ':'))
          || (not (Char.equal snippet.[after_index + 1] '_'))
          || not (Char.equal snippet.[after_index + 2] '}')
        then failwith (Printf.sprintf "unexpected snippet form: %S" snippet);
        Buffer.add_char buffer '_';
        loop (after_index + 3)
      | char ->
        Buffer.add_char buffer char;
        loop (index + 1))
  in
  loop 0
;;

let check_source fragments =
  let source = source fragments in
  let { Builder.snippet; placeholders = actual_count } = Builder.source ~source in
  let expected_count = List.sum (module Int) fragments ~f:placeholder_count in
  if actual_count <> expected_count
  then
    failwith
      (Printf.sprintf
         "placeholder count for %S: expected %d, got %d"
         source
         expected_count
         actual_count);
  let rendered = Snippet.to_string snippet in
  let round_trip = defaults rendered in
  if not (String.equal source round_trip)
  then
    failwith
      (Printf.sprintf
         "source round trip: expected %S, got %S via %S"
         source
         round_trip
         rendered)
;;

let source_examples =
  [ [ Expression_hole ]
  ; [ Function_with_wildcard ]
  ; [ Match_with_wildcards ]
  ; [ String_hole; Comment_hole; Quoted_string_hole; Hole_identifier ]
  ; [ String_dollar; String_close_brace; String_backslash; Tuple_holes ]
  ]
;;

let%test_unit "only expression holes become placeholders without changing source" =
  Test.run_exn (module Source_case) ~examples:source_examples ~f:check_source
;;

let print_source source =
  let { Builder.snippet; placeholders } = Builder.source ~source in
  Stdlib.Printf.printf "%d: %s\n" placeholders (Snippet.to_string snippet)
;;

let%expect_test "only expression holes are numbered, in source order" =
  print_source "(fun _ -> (_, _))";
  print_source "(match _ with | Some _ -> _ | None -> _)";
  print_source "(let _ = _ in _)";
  print_source "(_, _, _, _, _, _, _, _, _, _, _, _)";
  [%expect
    {|
    2: (fun _ -> (${1:_}, ${2:_}))$0
    3: (match ${1:_} with | Some _ -> ${2:_} | None -> ${3:_})$0
    2: (let _ = ${1:_} in ${2:_})$0
    12: (${1:_}, ${2:_}, ${3:_}, ${4:_}, ${5:_}, ${6:_}, ${7:_}, ${8:_}, ${9:_}, ${10:_}, ${11:_}, ${12:_})$0
    |}]
;;

let%expect_test "hole locations exclude surrounding syntax and type wildcards" =
  print_source "(( _ (* _ *) ))";
  print_source "(_ : _)";
  print_source "(let café = \"😀\" in (_, _))";
  print_source "[%merlin.hole]";
  [%expect
    {|
    1: (( ${1:_} (* _ *) ))$0
    1: (${1:_} : _)$0
    2: (let café = "😀" in (${1:_}, ${2:_}))$0
    0: [%merlin.hole]$0
    |}]
;;

let%expect_test "attributes on holes retain wildcard patterns and visit expression holes" =
  print_source "(_ [@foo? _])";
  print_source "_ [@foo _]";
  [%expect
    {|
    1: (${1:_} [@foo? _])$0
    2: ${1:_} [@foo ${2:_}]$0
    |}]
;;

let%expect_test "parse failures preserve source without placeholders" =
  List.iter
    [ ""; "(_"; "\"unterminated"; "(* unterminated"; {ocaml|("$}", _|ocaml} ]
    ~f:print_source;
  [%expect
    {|
    0: $0
    0: (_$0
    0: "unterminated$0
    0: (* unterminated$0
    0: ("\$\}", _$0
    |}]
;;

type argument =
  | Unlabelled
  | Labelled
  | Optional
  | Nested_arrow
  | Tuple
  | Object
  | Polymorphic_variant
  | Alias
  | Package
[@@deriving quickcheck, sexp_of]

module Application_case = struct
  type t = argument list [@@deriving quickcheck, sexp_of]
end

let argument = function
  | Unlabelled -> "int"
  | Labelled -> "f:(int -> int)"
  | Optional -> "?limit:int"
  | Nested_arrow -> "(string -> bool)"
  | Tuple -> "int * string"
  | Object -> "< m : int -> int >"
  | Polymorphic_variant -> "[ `A of (int -> int) | `B ]"
  | Alias -> "((int -> int) as 'fn)"
  | Package -> "(module S with type t = int)"
;;

let check_application arguments =
  let typ =
    match arguments with
    | [] -> "result"
    | _ -> String.concat ~sep:" -> " (List.map arguments ~f:argument @ [ "result" ])
  in
  let label_count =
    List.count arguments ~f:(function
      | Labelled | Optional -> true
      | _ -> false)
  in
  match Builder.application ~name:"f" ~typ, label_count >= 2 with
  | None, false -> ()
  | Some snippet, true ->
    let rendered = Snippet.to_string snippet in
    let expected_placeholders = List.length arguments in
    let { Builder.placeholders = actual_placeholders; _ } =
      Builder.source ~source:(defaults rendered)
    in
    if actual_placeholders <> expected_placeholders
    then
      failwith
        (Printf.sprintf
           "application %S: expected %d placeholders, got %d in %S"
           typ
           expected_placeholders
           actual_placeholders
           rendered)
  | None, true -> failwith (Printf.sprintf "missing application snippet for %S" typ)
  | Some snippet, false ->
    failwith
      (Printf.sprintf "unexpected application snippet %S" (Snippet.to_string snippet))
;;

let%test_unit "whole-call snippets require multiple labels and preserve arity" =
  Test.run_exn
    (module Application_case)
    ~examples:
      [ []
      ; [ Unlabelled ]
      ; [ Labelled ]
      ; [ Optional; Unlabelled ]
      ; [ Nested_arrow; Tuple ]
      ; [ Labelled; Optional ]
      ; [ Optional; Optional ]
      ; [ Nested_arrow
        ; Labelled
        ; Optional
        ; Tuple
        ; Object
        ; Polymorphic_variant
        ; Alias
        ; Package
        ]
      ]
    ~f:check_application
;;

let%expect_test "whole-call snippets" =
  let print name typ =
    match Builder.application ~name ~typ with
    | None -> Stdlib.print_endline "none"
    | Some snippet -> Snippet.to_string snippet |> Stdlib.print_endline
  in
  print "ListLabels.fold_left" "f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a";
  print "f" "name:string -> ?limit:int -> int -> string";
  print "f" "(int -> int) -> (int * string) -> result";
  print "f" "(a:int -> b:int -> int) as 'fn";
  print "f" "int";
  List.iter [ ""; "+"; "f..g"; "f$"; "~f" ] ~f:(fun name ->
    print name "a:int -> b:int -> int");
  print "f" "a:int -> b:int ->";
  [%expect
    {|
    ListLabels.fold_left ~f:${1:_} ~init:${2:_} ${3:_}$0
    f ~name:${1:_} ?limit:${2:_} ${3:_}$0
    none
    f ~a:${1:_} ~b:${2:_}$0
    none
    none
    none
    none
    none
    none
    none
    |}]
;;
