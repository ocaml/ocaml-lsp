open Async
open Test.Import

let apply_inlay_hints
  ?(path = "foo.ml")
  ?range
  ?(hint_function_params = false)
  ?(hint_pattern_variables = false)
  ?(hint_let_bindings = false)
  ?(hint_let_syntax_ppx = false)
  ~source
  ()
  =
  let range =
    match range with
    | Some r -> r
    | None ->
      let end_pos =
        let lines = String.split source ~on:'\n' in
        let last_line = Option.value_exn (List.last lines) in
        Position.create ~line:(List.length lines - 1) ~character:(String.length last_line)
      in
      Range.create ~start:(Position.create ~character:0 ~line:0) ~end_:end_pos
  in
  let uri = DocumentUri.of_path path in
  let request =
    let textDocument = TextDocumentIdentifier.create ~uri in
    InlayHintParams.create ~textDocument ~range ()
  in
  let%map inlay_hints =
    Test.run_request
      ~prep:(fun client -> Test.openDocument ~client ~uri ~source)
      ~settings:
        (`Assoc
          [ ( "inlayHints"
            , `Assoc
                [ "hintFunctionParams", `Bool hint_function_params
                ; "hintPatternVariables", `Bool hint_pattern_variables
                ; "hintLetBindings", `Bool hint_let_bindings
                ; "hintLetSyntaxPpx", `Bool hint_let_syntax_ppx
                ] )
          ])
      (InlayHint request)
  in
  match inlay_hints with
  | Some hints ->
    let text_edits =
      List.map hints ~f:(fun (hint : InlayHint.t) ->
        let paddingLeftStr =
          match hint.paddingLeft with
          | Some true -> "_$"
          | None | Some false -> "$"
        in
        let paddingRightStr =
          match hint.paddingRight with
          | Some true -> "$_"
          | None | Some false -> "$"
        in
        let newText =
          match hint.label with
          | `String s -> paddingLeftStr ^ s ^ paddingRightStr
          | `List _ -> failwith "TODO: implement list hints"
        in
        TextEdit.create
          ~range:(Range.create ~start:hint.position ~end_:hint.position)
          ~newText)
    in
    Test.apply_edits source text_edits |> print_endline
  | None -> print_endline "No hints found"
;;

let%expect_test "optional argument" =
  let%map () =
    apply_inlay_hints ~hint_function_params:true ~source:"let f ?x () = x" ()
  in
  [%expect {| let f ?x$: 'a option$ () = x |}]
;;

let%expect_test "optional argument with value" =
  let%map () =
    apply_inlay_hints ~hint_function_params:true ~source:"let f ?(x = 1) () = x" ()
  in
  [%expect {| let f ?(x$: int$ = 1) () = x |}]
;;

let%expect_test "labeled argument" =
  let%map () =
    apply_inlay_hints ~hint_function_params:true ~source:"let f ~x = x + 1" ()
  in
  [%expect {| let f ~x$: int$ = x + 1 |}]
;;

let%expect_test "case argument" =
  let%map () =
    apply_inlay_hints ~hint_function_params:true ~source:"let f (Some x) = x + 1" ()
  in
  [%expect {| let f (Some x$: int$) = x + 1 |}]
;;

let%expect_test "pattern variables" =
  let source = "let f x = match x with Some x -> x | None -> 0" in
  let%bind () = apply_inlay_hints ~hint_function_params:true ~source () in
  [%expect {| let f x$: int option$ = match x with Some x -> x | None -> 0 |}];
  let%map () =
    apply_inlay_hints ~hint_function_params:true ~hint_pattern_variables:true ~source ()
  in
  [%expect {| let f x$: int option$ = match x with Some x$: int$ -> x | None -> 0 |}]
;;

let%expect_test "let bindings" =
  let source = "let f () = let y = 0 in y" in
  let%bind () = apply_inlay_hints ~hint_function_params:true ~source () in
  [%expect {| let f () = let y = 0 in y |}];
  let%map () =
    apply_inlay_hints ~hint_function_params:true ~hint_let_bindings:true ~source ()
  in
  [%expect {| let f () = let y$: int$ = 0 in y |}]
;;

let%expect_test "everything off" =
  let%map () =
    apply_inlay_hints
      ~source:
        {|
let foo x y =
  let z = x, y in
  match z with
  | 0, b -> b
  | _ -> 0
;;|}
      ()
  in
  [%expect
    {|
    let foo x y =
      let z = x, y in
      match z with
      | 0, b -> b
      | _ -> 0
    ;;
    |}]
;;

let%expect_test "function params only" =
  let%map () =
    apply_inlay_hints
      ~hint_function_params:true
      ~source:
        {|
let foo x y =
  let z = x, y in
  match z with
  | 0, b -> b
  | _ -> 0
;;|}
      ()
  in
  [%expect
    {|
    let foo x$: int$ y$: int$ =
      let z = x, y in
      match z with
      | 0, b -> b
      | _ -> 0
    ;;
    |}]
;;

let%expect_test "let bindings only" =
  let%map () =
    apply_inlay_hints
      ~hint_let_bindings:true
      ~source:
        {|
let foo x y =
  let z = x, y in
  match z with
  | 0, b -> b
  | _ -> 0
;;|}
      ()
  in
  [%expect
    {|
    let foo x y =
      let z$: int * int$ = x, y in
      match z with
      | 0, b -> b
      | _ -> 0
    ;;
    |}]
;;

let%expect_test "pattern variables only" =
  let%map () =
    apply_inlay_hints
      ~hint_pattern_variables:true
      ~source:
        {|
let foo x y =
  let z = x, y in
  match z with
  | 0, b -> b
  | _ -> 0
;;|}
      ()
  in
  [%expect
    {|
    let foo x y =
      let z = x, y in
      match z with
      | 0, b$: int$ -> b
      | _ -> 0
    ;;
    |}]
;;

let%expect_test "everything on" =
  let%map () =
    apply_inlay_hints
      ~hint_let_bindings:true
      ~hint_pattern_variables:true
      ~hint_function_params:true
      ~source:
        {|
let foo x y =
  let z = x, y in
  match z with
  | 0, b -> b
  | _ -> 0
;;|}
      ()
  in
  [%expect
    {|
    let foo x$: int$ y$: int$ =
      let z$: int * int$ = x, y in
      match z with
      | 0, b$: int$ -> b
      | _ -> 0
    ;;
    |}]
;;

let%expect_test "argument to function that uses [function]" =
  let%bind () =
    apply_inlay_hints
      ~source:
        {|
let bar a = function
  | 0 -> 10
  | _ -> -a
;;|}
      ()
  in
  [%expect
    {|
    let bar a = function
      | 0 -> 10
      | _ -> -a
    ;;
    |}];
  let%map () =
    apply_inlay_hints
      ~hint_function_params:true
      ~source:
        {|
    let bar a = function
      | 0 -> 10
      | _ -> -a
    ;;|}
      ()
  in
  [%expect
    {|
    let bar a$: int$ = function
      | 0 -> 10
      | _ -> -a
    ;;
    |}]
;;

let%expect_test "pattern variables in function that uses [function]" =
  let%bind () =
    apply_inlay_hints
      ~source:
        {|
let baz = function
  | 0, b -> b
  | _ -> 0
;;|}
      ()
  in
  [%expect
    {|
    let baz = function
      | 0, b -> b
      | _ -> 0
    ;;
    |}];
  let%map () =
    apply_inlay_hints
      ~hint_pattern_variables:true
      ~source:
        {|
let baz = function
  | 0, b -> b
  | _ -> 0
;;|}
      ()
  in
  [%expect
    {|
    let baz = function
      | 0, b$: int$ -> b
      | _ -> 0
    ;;
    |}]
;;

let%expect_test "let-syntax ppx annotations" =
  (* Note: We can't test cases where we interleave [let open _] with the PPX because we
     don't interpret them when running the test and so at the type stage the opens don't
     exist. *)
  let source =
    {|
      module First = struct module Let_syntax = struct end end
      module Second = struct module Let_syntax = struct end end
      module Reference = First

      open First

      let _ =
        let%bind () = return () in
        let%map () = return ()
      ;;

      let _ =
        let open Second in
        let%bind () = return () in
        let%map () = return () in
        return ()
      ;;

      open Reference

      let _ =
        let%bind () = return () in
        let%map () = return () in
        return ()
      ;;

      module Top_level = struct
        module Nested = struct
          module Let_syntax = struct end
        end
      end

      open Top_level

      let _ =
        let open Nested in
        let%bind () = return () in
        let%map () = return () in
        return ()
      ;;
    |}
  in
  let%bind () = apply_inlay_hints ~hint_let_syntax_ppx:true ~source () in
  [%expect
    {|
    module First = struct module Let_syntax = struct end end
    module Second = struct module Let_syntax = struct end end
    module Reference = First

    open First

    let _ =
      let%bind$.Reference$ () = return () in
      let%map$.Reference$ () = return ()
    ;;

    let _ =
      let open Second in
      let%bind$.Second$ () = return () in
      let%map$.Second$ () = return () in
      return ()
    ;;

    open Reference

    let _ =
      let%bind$.Reference$ () = return () in
      let%map$.Reference$ () = return () in
      return ()
    ;;

    module Top_level = struct
      module Nested = struct
        module Let_syntax = struct end
      end
    end

    open Top_level

    let _ =
      let open Nested in
      let%bind$.Nested$ () = return () in
      let%map$.Nested$ () = return () in
      return ()
    ;;
    |}];
  return ()
;;
