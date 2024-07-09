open Test.Import

let apply_inlay_hints
  ?(path = "foo.ml")
  ?range
  ?(hint_pattern_variables = false)
  ?(hint_let_bindings = false)
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
  let inlay_hints =
    Test.run_request
      ~prep:(fun client -> Test.openDocument ~client ~uri ~source)
      ~settings:
        (`Assoc
          [ ( "inlayHints"
            , `Assoc
                [ "hintPatternVariables", `Bool hint_pattern_variables
                ; "hintLetBindings", `Bool hint_let_bindings
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
  apply_inlay_hints ~source:"let f ?x () = x" ();
  [%expect {| let f ?x$: 'a option$ () = x |}]
;;

let%expect_test "optional argument with value" =
  apply_inlay_hints ~source:"let f ?(x = 1) () = x" ();
  [%expect {| let f ?(x$: int$ = 1) () = x |}]
;;

let%expect_test "labeled argument" =
  apply_inlay_hints ~source:"let f ~x = x + 1" ();
  [%expect {| let f ~x$: int$ = x + 1 |}]
;;

let%expect_test "case argument" =
  apply_inlay_hints ~source:"let f (Some x) = x + 1" ();
  [%expect {| let f (Some x$: int$) = x + 1 |}]
;;

let%expect_test "pattern variables" =
  let source = "let f x = match x with Some x -> x | None -> 0" in
  apply_inlay_hints ~source ();
  [%expect {| let f x$: int option$ = match x with Some x -> x | None -> 0 |}];
  apply_inlay_hints ~hint_pattern_variables:true ~source ();
  [%expect {| let f x$: int option$ = match x with Some x$: int$ -> x | None -> 0 |}]
;;

let%expect_test "let bindings" =
  let source = "let f () = let y = 0 in y" in
  apply_inlay_hints ~source ();
  [%expect {| let f () = let y = 0 in y |}];
  apply_inlay_hints ~hint_let_bindings:true ~source ();
  [%expect {| let f () = let y$: int$ = 0 in y |}]
;;
