open Test.Import

let print_diagnostics
      ?(prep = fun _ -> Fiber.return ())
      ?(print_range = false)
      ?(path = "foo.ml")
      source
  =
  Lsp_helpers.open_document_with_diagnostics_callback
    ~prep
    ~path
    ~source
    ~diagnostics_callback:(fun diagnostics ->
      print_endline
        (String.concat
           ~sep:", "
           (List.map diagnostics.diagnostics ~f:(fun (d : Diagnostic.t) ->
              let range_message =
                if print_range
                then "\n" ^ Ocaml_lsp_server.Testing.Range.to_string d.range ^ "\n"
                else ""
              in
              match d.message with
              | `String m -> m ^ range_message
              | `MarkupContent { value; _ } -> value ^ range_message))))
    ()
;;

let change_config client params = Client.notification client (ChangeConfiguration params)

let%expect_test "shorten diagnostics - true" =
  let source =
    {ocaml|
    let x: unit = fun () ->




   ()

   let () = match true with

   | false -> ()
|ocaml}
  in
  print_diagnostics
    ~prep:(fun client ->
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:
             (`Assoc [ "shortenMerlinDiagnostics", `Assoc [ "enable", `Bool true ] ])))
    ~print_range:true
    source;
  [%expect
    {|
    This expression should not be a function, the expected type is
    unit
    ((1, 18), (2, 0))
    , Warning 8: this pattern-matching is not exhaustive.
      Here is an example of a case that is not matched: true
    ((8, 12), (9, 0))
    |}]
;;

let%expect_test "shorten diagnostics - false" =
  let source =
    {ocaml|
    let x: unit = fun () ->




   ()

   let () = match true with

   | false -> ()
|ocaml}
  in
  print_diagnostics
    ~prep:(fun client ->
      change_config
        client
        (DidChangeConfigurationParams.create
           ~settings:
             (`Assoc [ "shortenMerlinDiagnostics", `Assoc [ "enable", `Bool false ] ])))
    ~print_range:true
    source;
  [%expect
    {|
    This expression should not be a function, the expected type is
    unit
    ((1, 18), (6, 5))
    , Warning 8: this pattern-matching is not exhaustive.
      Here is an example of a case that is not matched: true
    ((8, 12), (10, 16))
    |}]
;;
