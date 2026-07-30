open Test.Import

let make_capabilities
      ?activeParameterSupport
      ?contextSupport
      ?labelOffsetSupport
      ?noActiveParameterSupport
      ()
  =
  let parameterInformation =
    ClientSignatureParameterInformationOptions.create ?labelOffsetSupport ()
  in
  let signatureInformation =
    ClientSignatureInformationOptions.create
      ?activeParameterSupport
      ~documentationFormat:[ MarkupKind.Markdown; MarkupKind.PlainText ]
      ?noActiveParameterSupport
      ~parameterInformation
      ()
  in
  let signatureHelp =
    SignatureHelpClientCapabilities.create
      ?contextSupport
      ~dynamicRegistration:true
      ~signatureInformation
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~signatureHelp () in
  ClientCapabilities.create ~textDocument ()
;;

let capabilities = make_capabilities ~labelOffsetSupport:true ()

let signature_help ?context client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (SignatureHelp (SignatureHelpParams.create ?context ~textDocument ~position ()))
;;

let print_signature_help signature_help =
  let json = SignatureHelp.yojson_of_t signature_help in
  let documentation =
    List.find_map
      signature_help.signatures
      ~f:(fun (signature : SignatureInformation.t) ->
        match signature.documentation with
        | None -> None
        | Some (`String value | `MarkupContent { value; _ }) -> Some value)
  in
  let output =
    match documentation with
    | None -> Yojson.Safe.pretty_to_string ~std:false json
    | Some documentation ->
      let placeholder = "__SIGNATURE_HELP_DOCUMENTATION__" in
      let rec replace_documentation = function
        | `String value when String.equal value documentation -> `String placeholder
        | `Assoc fields ->
          `Assoc
            (List.map fields ~f:(fun (name, value) -> name, replace_documentation value))
        | `List values -> `List (List.map values ~f:replace_documentation)
        | json -> json
      in
      let output =
        replace_documentation json |> Yojson.Safe.pretty_to_string ~std:false
      in
      Re.replace_string
        (Re.compile (Re.str ("\"" ^ placeholder ^ "\"")))
        ~by:("\"" ^ documentation ^ "\"")
        output
  in
  print_endline output
;;

let test ?(capabilities = capabilities) source position =
  Helpers.test ~capabilities source (fun client ->
    let* response = signature_help client position in
    print_signature_help response;
    Fiber.return ())
;;

let%expect_test "signature help inside a comment after Unicode" =
  let source = "let add a b = a + b\nlet _ = \"😀😀\"; add 1 (* x *)" in
  test source (Position.create ~line:1 ~character:24);
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "add : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}]
;;

let nullable_int_to_yojson = function
  | None -> `String "<omitted>"
  | Some None -> `Null
  | Some (Some value) -> `Int value
;;

let print_active_parameters (help : SignatureHelp.t) =
  match help.signatures with
  | [] -> print_endline "<no signatures>"
  | signature :: _ ->
    Test.print_result
      (`Assoc
          [ "SignatureHelp.activeParameter", nullable_int_to_yojson help.activeParameter
          ; ( "SignatureInformation.activeParameter"
            , nullable_int_to_yojson signature.activeParameter )
          ])
;;

let%expect_test "parameter label representation follows client capabilities" =
  let source = "let map = ListLabels.map\n\nlet _ = map" in
  let position = Position.create ~line:2 ~character:11 in
  let check description labelOffsetSupport =
    print_endline description;
    let capabilities = make_capabilities ?labelOffsetSupport () in
    Helpers.test ~capabilities source (fun client ->
      let* help = signature_help client position in
      let labels =
        List.concat_map help.signatures ~f:(fun signature ->
          Option.value ~default:[] signature.parameters)
        |> List.map ~f:(fun parameter ->
          match parameter.ParameterInformation.label with
          | `String label -> `String label
          | `Offset (start, end_) -> `List [ `Int start; `Int end_ ])
      in
      Test.print_result (`List labels);
      Fiber.return ())
  in
  check "labelOffsetSupport omitted" None;
  [%expect
    {|
    labelOffsetSupport omitted
    [ [ 6, 18 ], [ 22, 29 ] ]
    |}];
  check "labelOffsetSupport false" (Some false);
  [%expect
    {|
    labelOffsetSupport false
    [ [ 6, 18 ], [ 22, 29 ] ]
    |}];
  check "labelOffsetSupport true" (Some true);
  [%expect
    {|
    labelOffsetSupport true
    [ [ 6, 18 ], [ 22, 29 ] ]
    |}]
;;

let%expect_test "active parameter placement follows client capabilities" =
  let source = "let map = ListLabels.map\n\nlet _ = map []" in
  let position = Position.create ~line:2 ~character:14 in
  let check description activeParameterSupport =
    print_endline description;
    let capabilities = make_capabilities ?activeParameterSupport () in
    Helpers.test ~capabilities source (fun client ->
      let* help = signature_help client position in
      print_active_parameters help;
      Fiber.return ())
  in
  check "activeParameterSupport omitted" None;
  [%expect
    {|
    activeParameterSupport omitted
    {
      "SignatureHelp.activeParameter": 1,
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}];
  check "activeParameterSupport false" (Some false);
  [%expect
    {|
    activeParameterSupport false
    {
      "SignatureHelp.activeParameter": 1,
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}];
  check "activeParameterSupport true" (Some true);
  [%expect
    {|
    activeParameterSupport true
    {
      "SignatureHelp.activeParameter": 1,
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}]
;;

let%expect_test "no active parameter follows client capabilities" =
  let source = "let f ~foo = foo\nlet _ = f" in
  let position = Position.create ~line:1 ~character:9 in
  let check description ?activeParameterSupport ?noActiveParameterSupport () =
    print_endline description;
    let capabilities =
      make_capabilities ?activeParameterSupport ?noActiveParameterSupport ()
    in
    Helpers.test ~capabilities source (fun client ->
      let* help = signature_help client position in
      print_active_parameters help;
      Fiber.return ())
  in
  check "capabilities omitted" ();
  [%expect
    {|
    capabilities omitted
    {
      "SignatureHelp.activeParameter": "<omitted>",
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}];
  check
    "legacy non-null client"
    ~activeParameterSupport:false
    ~noActiveParameterSupport:false
    ();
  [%expect
    {|
    legacy non-null client
    {
      "SignatureHelp.activeParameter": "<omitted>",
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}];
  check
    "legacy nullable client"
    ~activeParameterSupport:false
    ~noActiveParameterSupport:true
    ();
  [%expect
    {|
    legacy nullable client
    {
      "SignatureHelp.activeParameter": "<omitted>",
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}];
  check
    "modern non-null client"
    ~activeParameterSupport:true
    ~noActiveParameterSupport:false
    ();
  [%expect
    {|
    modern non-null client
    {
      "SignatureHelp.activeParameter": "<omitted>",
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}];
  check
    "modern nullable client"
    ~activeParameterSupport:true
    ~noActiveParameterSupport:true
    ();
  [%expect
    {|
    modern nullable client
    {
      "SignatureHelp.activeParameter": "<omitted>",
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}]
;;

let%expect_test "signature help request contexts" =
  let source = "let add a b = a + b\nlet _ = add 1 " in
  let position = Position.create ~line:1 ~character:14 in
  let capabilities = make_capabilities ~contextSupport:true () in
  Helpers.test ~capabilities source (fun client ->
    let request description context =
      print_endline description;
      let* help = signature_help ~context client position in
      print_active_parameters help;
      Fiber.return help
    in
    let invoked =
      SignatureHelpContext.create
        ~isRetrigger:false
        ~triggerKind:SignatureHelpTriggerKind.Invoked
        ()
    in
    let* active = request "invoked" invoked in
    let triggered =
      SignatureHelpContext.create
        ~isRetrigger:false
        ~triggerCharacter:" "
        ~triggerKind:SignatureHelpTriggerKind.TriggerCharacter
        ()
    in
    let* (_ : SignatureHelp.t) = request "trigger character" triggered in
    let retriggered =
      SignatureHelpContext.create
        ~activeSignatureHelp:active
        ~isRetrigger:true
        ~triggerKind:SignatureHelpTriggerKind.ContentChange
        ()
    in
    let* (_ : SignatureHelp.t) = request "content-change retrigger" retriggered in
    Fiber.return ());
  [%expect
    {|
    invoked
    {
      "SignatureHelp.activeParameter": 1,
      "SignatureInformation.activeParameter": "<omitted>"
    }
    trigger character
    {
      "SignatureHelp.activeParameter": 1,
      "SignatureInformation.activeParameter": "<omitted>"
    }
    content-change retrigger
    {
      "SignatureHelp.activeParameter": 1,
      "SignatureInformation.activeParameter": "<omitted>"
    }
    |}]
;;

let%expect_test "signature help retrigger after a nested application" =
  let source =
    "let add a b = a + b\nlet take3 a b c = a + b + c\nlet _ = take3 1 (add 2 3) "
  in
  let capabilities = make_capabilities ~contextSupport:true () in
  Helpers.test ~capabilities source (fun client ->
    let* active = signature_help client (Position.create ~line:2 ~character:23) in
    print_endline "before closing nested application";
    print_active_parameters active;
    let context =
      SignatureHelpContext.create
        ~activeSignatureHelp:active
        ~isRetrigger:true
        ~triggerCharacter:")"
        ~triggerKind:SignatureHelpTriggerKind.TriggerCharacter
        ()
    in
    let* help = signature_help ~context client (Position.create ~line:2 ~character:25) in
    print_endline "after closing nested application";
    print_active_parameters help;
    Fiber.return ());
  [%expect
    {|
    before closing nested application
    {
      "SignatureHelp.activeParameter": 1,
      "SignatureInformation.activeParameter": "<omitted>"
    }
    after closing nested application
    <no signatures>
    |}]
;;

let%expect_test "can provide signature help after a function-type value" =
  let source =
    {ocaml|let map = ListLabels.map

let _ = map
|ocaml}
  in
  test source (Position.create ~line:2 ~character:11);
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "map : f:('a -> 'b) -> 'a list -> 'b list",
          "parameters": [ { "label": [ 6, 18 ] }, { "label": [ 22, 29 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "can provide signature help for an operator" =
  let source =
    {ocaml|let (+) = (+)

let _ = 1 + 2
|ocaml}
  in
  test source (Position.create ~line:2 ~character:13);
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "(+) : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "can provide signature help for an anonymous function" =
  let source =
    {ocaml|let _ = (fun x -> x + 1)
|ocaml}
  in
  test source (Position.create ~line:0 ~character:26);
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        { "label": "_ : int -> int", "parameters": [ { "label": [ 4, 7 ] } ] }
      ]
    }
    |}]
;;

let%expect_test "can make the non-labelled parameter active" =
  let source =
    {ocaml|let map = ListLabels.map

let _ = map []
|ocaml}
  in
  test source (Position.create ~line:2 ~character:14);
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "map : f:('a -> 'b) -> 'a list -> 'b list",
          "parameters": [ { "label": [ 6, 18 ] }, { "label": [ 22, 29 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "can make the labelled parameter active" =
  let source =
    {ocaml|let map = ListLabels.map

let _ = map ~f:Int.abs
|ocaml}
  in
  test source (Position.create ~line:2 ~character:22);
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "map : f:(int -> int) -> int list -> int list",
          "parameters": [ { "label": [ 6, 20 ] }, { "label": [ 24, 32 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "can make a labelled parameter active by prefix" =
  let source =
    {ocaml|let mem = ListLabels.mem

let _ = mem ~se
|ocaml}
  in
  test source (Position.create ~line:2 ~character:15);
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "mem : 'a -> set:'a list -> bool",
          "parameters": [ { "label": [ 6, 8 ] }, { "label": [ 12, 23 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "can make an optional parameter active by prefix" =
  let source =
    {ocaml|let create = Hashtbl.create

let _ = create ?ra
|ocaml}
  in
  test source (Position.create ~line:2 ~character:18);
  [%expect
    {|
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "create : ?random:bool -> int -> ('a, 'b) Hashtbl.t",
          "parameters": [ { "label": [ 9, 21 ] }, { "label": [ 25, 28 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "can return documentation for the function being applied" =
  let source =
    {ocaml|(** This is an example of a docstring that demonstrates various ocamldoc syntax features.

    {3 Sections and Labels}

    We can create sections using {3 Section title} and labels using {3:label_name Section title with label}

    {3 Links and Cross-references}

    External links: {{:https://ocaml.org/} OCaml's official website}

    Cross-references: {!List.length} {{!List.length} Replacement text}

    {3 Inline Formatting}

    {b Bold}, {i Italic}, {e Emphasize}, {^ Superscript}, {_ Subscript}, and [inline code]

    {3 Text Alignment}

    {C Centered text}
    {L Left-aligned text}
    {R Right-aligned text}

    {3 Lists}

    {ol
    {- Ordered list item 1}
    {- Ordered list item 2}
    }

    {ul
    {- Unordered list item 1}
    {- Unordered list item 2}
    }

    - Unordered list item 1
    - Unordered list item 2

    {3 Code Blocks}

    {[
      let square x = x * x
      let result = square 3
    ]}

    {@python[
    def f():
      return 0
    ]}

    {3 Verbatim}

    {v
    This text will be displayed verbatim.
    No formatting will be applied.
    v}

    {3 Module List}

    {!modules: Array List String}

    @param x dividend
    @param divisor

    @return {i quotient}, i.e. result of division
    @raise Division_by_zero raised when divided by zero

    @see <https://en.wikipedia.org/wiki/Arithmetic#Division_(%C3%B7,_or_/)> article
    @see 'arithmetic.ml' for more context

    @since 4.0.0
    @before 4.4.0

    @deprecated use [(/)]

    @version 1.0.0
    @author John Doe *)
let div x y =
  x / y

let _ = div 1
|ocaml}
  in
  test source (Position.create ~line:80 ~character:13);
  [%expect
    {|
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "documentation": {
            "kind": "markdown",
            "value": "This is an example of a docstring that demonstrates various ocamldoc syntax features.

    #### Sections and Labels

    We can create sections using

    #### Section title

    and labels using

    #### Section title with label

    #### Links and Cross-references

    External links: [OCaml's official website](https://ocaml.org/)

    Cross-references: `List.length` Replacement text

    #### Inline Formatting

    **Bold**, *Italic*, *Emphasize*, ^{Superscript}, \_{Subscript}, and `inline code`

    #### Text Alignment

    Centered text

    Left-aligned text

    Right-aligned text

    #### Lists

    1. Ordered list item 1
    2. Ordered list item 2

    - Unordered list item 1
    - Unordered list item 2

    - Unordered list item 1
    - Unordered list item 2

    #### Code Blocks

    ```ocaml
    let square x = x * x
    let result = square 3
    ```

    ```python
    def f():
      return 0
    ```

    #### Verbatim

    ```verb
        This text will be displayed verbatim.
        No formatting will be applied.
    ```

    #### Module List

    * Array
    * List
    * String

    ***@param*** `x`
    dividend

    ***@param*** divisor

    ***@return***
    *quotient*, i.e. result of division

    ***@raise*** `Division_by_zero`
    raised when divided by zero

    ***@see*** [link](https://en.wikipedia.org/wiki/Arithmetic#Division_\(%C3%B7,_or_/\))
    article

    ***@see*** `arithmetic.ml`
    for more context

    ***@since*** `4.0.0`

    ***@before*** `4.4.0`

    ***@deprecated***
    use `(/)`

    ***@version*** `1.0.0`

    ***@author*** John Doe"
          },
          "label": "div : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}]
;;

let%expect_test "signature help after a completed application or closed scope" =
  let source = "let add a b = a + b in \n \nadd 1 1;; \n " in
  let check description position =
    print_endline description;
    test source position
  in
  check "after in" (Position.create ~line:0 ~character:23);
  [%expect
    {|
    after in
    { "signatures": [] }
    |}];
  check "on blank line before application" (Position.create ~line:1 ~character:1);
  [%expect
    {|
    on blank line before application
    { "signatures": [] }
    |}];
  check "after completed application" (Position.create ~line:2 ~character:10);
  [%expect
    {|
    after completed application
    { "signatures": [] }
    |}];
  check "on blank line after terminator" (Position.create ~line:3 ~character:1);
  [%expect
    {|
    on blank line after terminator
    { "signatures": [] }
    |}]
;;

let rec censor_backtraces = function
  | `Assoc fields ->
    `Assoc
      (List.map fields ~f:(fun (name, value) ->
         if String.equal name "backtrace"
         then name, `String "<censored>"
         else name, censor_backtraces value))
  | `List values -> `List (List.map values ~f:censor_backtraces)
  | json -> json
;;

let%expect_test "malformed Unicode application returns no signature help" =
  Helpers.test "a>😀" (fun client ->
    let* result =
      Fiber.collect_errors (fun () ->
        signature_help client (Position.create ~line:0 ~character:1))
    in
    match result with
    | Error [ { Exn_with_backtrace.exn = Jsonrpc.Response.Error.E error; backtrace = _ } ]
      ->
      Jsonrpc.Response.Error.yojson_of_t error |> censor_backtraces |> Test.print_result;
      Fiber.return ()
    | Error errors -> Fiber.reraise_all errors
    | Ok response ->
      print_signature_help response;
      Fiber.return ());
  [%expect {| { "signatures": [] } |}]
;;

let%expect_test "signature help after expression boundaries" =
  let check description source position =
    print_endline description;
    test source position
  in
  check
    "after sequence separator"
    "let add a b = a + b\nlet _ = add 1; "
    (Position.create ~line:1 ~character:15);
  [%expect
    {|
    after sequence separator
    { "signatures": [] }
    |}];
  check
    "after toplevel terminator"
    "let add a b = a + b\nlet _ = add 1;; "
    (Position.create ~line:1 ~character:16);
  [%expect
    {|
    after toplevel terminator
    { "signatures": [] }
    |}];
  check
    "after local binding"
    "let add a b = a + b\nlet _ =\n  let partial = add 1 in "
    (Position.create ~line:2 ~character:25);
  [%expect
    {|
    after local binding
    { "signatures": [] }
    |}];
  check
    "after tuple separator"
    "let add a b = a + b\nlet _ = add 1, "
    (Position.create ~line:1 ~character:15);
  [%expect
    {|
    after tuple separator
    { "signatures": [] }
    |}];
  check
    "after then"
    "let add a b = a + b\nlet _ = if add 1 then "
    (Position.create ~line:1 ~character:22);
  [%expect
    {|
    after then
    { "signatures": [] }
    |}];
  check
    "after else"
    "let add a b = a + b\nlet _ = if true then add 1 else "
    (Position.create ~line:1 ~character:32);
  [%expect
    {|
    after else
    { "signatures": [] }
    |}];
  check
    "after match scrutinee"
    "let add a b = a + b\nlet _ = match add 1 with "
    (Position.create ~line:1 ~character:25);
  [%expect
    {|
    after match scrutinee
    { "signatures": [] }
    |}];
  check
    "after next match case"
    "let add a b = a + b\nlet _ =\n  match () with\n  | () -> add 1\n  | _ -> "
    (Position.create ~line:4 ~character:9);
  [%expect
    {|
    after next match case
    { "signatures": [] }
    |}]
;;

let%expect_test "signature help remains relevant around nested syntax" =
  let check description source position =
    print_endline description;
    test source position
  in
  check
    "after separator-like comment"
    "let add a b = a + b\nlet _ = add 1 (* ; ;; *) "
    (Position.create ~line:1 ~character:25);
  [%expect
    {|
    after separator-like comment
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "add : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}];
  check
    "after nested sequence argument"
    "let take3 a b c = a + b + c\nlet _ = take3 (let x = 1 in x; x) 2 "
    (Position.create ~line:1 ~character:36);
  [%expect
    {|
    after nested sequence argument
    {
      "activeParameter": 2,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "take3 : int -> int -> int -> int",
          "parameters": [
            { "label": [ 8, 11 ] },
            { "label": [ 15, 18 ] },
            { "label": [ 22, 25 ] }
          ]
        }
      ]
    }
    |}];
  check
    "before closing parenthesis"
    "let add a b = a + b\nlet _ = (add 1 )"
    (Position.create ~line:1 ~character:15);
  [%expect
    {|
    before closing parenthesis
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "add : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}];
  check
    "after closing parenthesis"
    "let add a b = a + b\nlet _ = (add 1) "
    (Position.create ~line:1 ~character:16);
  [%expect
    {|
    after closing parenthesis
    {
      "activeParameter": 0,
      "activeSignature": 0,
      "signatures": [
        { "label": "_ : int -> int", "parameters": [ { "label": [ 4, 7 ] } ] }
      ]
    }
    |}];
  check
    "inside nested application"
    "let add a b = a + b\nlet _ = add 1 (add 2 )"
    (Position.create ~line:1 ~character:21);
  [%expect
    {|
    inside nested application
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "add : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}];
  check
    "after completed nested application"
    "let add a b = a + b\nlet take3 a b c = a + b + c\nlet _ = take3 1 (add 2 3) "
    (Position.create ~line:2 ~character:26);
  [%expect
    {|
    after completed nested application
    {
      "activeParameter": 2,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "take3 : int -> int -> int -> int",
          "parameters": [
            { "label": [ 8, 11 ] },
            { "label": [ 15, 18 ] },
            { "label": [ 22, 25 ] }
          ]
        }
      ]
    }
    |}];
  check
    "after infix operator"
    "let ( ++ ) a b = a + b\nlet _ = 1 ++ "
    (Position.create ~line:1 ~character:13);
  [%expect
    {|
    after infix operator
    {
      "activeSignature": 0,
      "signatures": [
        {
          "label": "(++) : int -> int -> int",
          "parameters": [ { "label": [ 7, 10 ] }, { "label": [ 14, 17 ] } ]
        }
      ]
    }
    |}];
  check
    "between existing arguments"
    "let add a b = a + b\nlet _ = add 1 2"
    (Position.create ~line:1 ~character:14);
  [%expect
    {|
    between existing arguments
    {
      "activeParameter": 1,
      "activeSignature": 0,
      "signatures": [
        {
          "label": "add : int -> int -> int",
          "parameters": [ { "label": [ 6, 9 ] }, { "label": [ 13, 16 ] } ]
        }
      ]
    }
    |}]
;;
