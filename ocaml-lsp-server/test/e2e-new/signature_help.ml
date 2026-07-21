open Test.Import

let capabilities =
  let parameterInformation =
    ClientSignatureParameterInformationOptions.create ~labelOffsetSupport:true ()
  in
  let signatureInformation =
    ClientSignatureInformationOptions.create
      ~documentationFormat:[ MarkupKind.Markdown; MarkupKind.PlainText ]
      ~parameterInformation
      ()
  in
  let signatureHelp =
    SignatureHelpClientCapabilities.create
      ~dynamicRegistration:true
      ~signatureInformation
      ()
  in
  let textDocument = TextDocumentClientCapabilities.create ~signatureHelp () in
  ClientCapabilities.create ~textDocument ()
;;

let signature_help client position =
  let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
  Client.request
    client
    (SignatureHelp (SignatureHelpParams.create ~textDocument ~position ()))
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

let test source position =
  Helpers.test ~capabilities source (fun client ->
    let* response = signature_help client position in
    print_signature_help response;
    Fiber.return ())
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
