open Test.Import

let default_name = function
  | `Local -> "var_name"
  | `Function -> "fun_name"
;;

(* Decode only the grammar emitted by extraction: escaped literal text, two
   occurrences of placeholder 1, and a final tabstop. This checks protocol text,
   not a real editor's snippet implementation. *)
let expand ~default ~name snippet =
  let placeholder = "${1:" ^ default ^ "}" in
  let buffer = Buffer.create (String.length snippet) in
  let names = ref 0
  and final = ref 0 in
  let matches pos text =
    let length = String.length text in
    pos + length <= String.length snippet
    && String.equal (String.sub snippet ~pos ~len:length) text
  in
  let rec loop pos =
    if pos < String.length snippet
    then (
      match snippet.[pos] with
      | '\\' ->
        assert (pos + 1 < String.length snippet);
        let escaped = snippet.[pos + 1] in
        assert (List.mem [ '\\'; '$'; '}' ] escaped ~equal:Char.equal);
        Buffer.add_char buffer escaped;
        loop (pos + 2)
      | '$' when matches pos placeholder ->
        incr names;
        Buffer.add_string buffer name;
        loop (pos + String.length placeholder)
      | '$' when matches pos "$0" ->
        incr final;
        assert (pos + 2 = String.length snippet)
      | '$' -> failwith "unexpected snippet syntax"
      | char ->
        Buffer.add_char buffer char;
        loop (pos + 1))
  in
  loop 0;
  assert (!names = 2 && !final = 1);
  Buffer.contents buffer
;;

let request ?path ~capabilities ~kind ~source ~range () =
  let title =
    match kind with
    | `Local -> "Extract local"
    | `Function -> "Extract function"
  in
  let result = ref None in
  Code_actions.iter_code_actions
    ?path
    ~capabilities
    ~only:[ RefactorExtract ]
    ~source
    range
    (fun actions ->
       result
       := Option.bind
            actions
            ~f:
              (List.find_map ~f:(function
                 | `CodeAction (action : CodeAction.t)
                   when String.equal action.title title ->
                   assert (action.kind = Some RefactorExtract);
                   assert (action.command = None && action.isPreferred = Some false);
                   let edit = Option.value_exn action.edit in
                   assert (Option.is_none edit.changes);
                   (match edit.documentChanges with
                    | Some [ `TextDocumentEdit { textDocument; edits } ] ->
                      assert (textDocument.version = Some 0);
                      Some edits
                    | _ -> failwith "expected one versioned document edit")
                 | _ -> None)));
  !result
;;

let apply ?(position_encoding = `UTF16) ~source edits =
  let textDocument =
    TextDocumentItem.create
      ~uri:Helpers.uri
      ~version:0
      ~languageId:(Other "ocaml")
      ~text:source
  in
  let doc =
    Lsp.Text_document.make
      ~position_encoding
      (DidOpenTextDocumentParams.create ~textDocument)
  in
  Lsp.Text_document.apply_text_document_edits doc edits |> Lsp.Text_document.text
;;

let plain_edits = function
  | [ `TextEdit a; `TextEdit b ] -> [ a; b ]
  | _ -> failwith "expected two ordinary edits"
;;

let check ?path ?name ~kind ~source ~range () =
  let plain =
    request ?path ~capabilities:(ClientCapabilities.create ()) ~kind ~source ~range ()
  in
  let snippet =
    request
      ?path
      ~capabilities:Code_actions.snippet_edit_capabilities
      ~kind
      ~source
      ~range
      ()
  in
  match plain, snippet with
  | None, None -> None
  | Some plain, Some [ `SnippetTextEdit edit ] ->
    let default = default_name kind in
    let expand name =
      let newText = expand ~default ~name edit.snippet.value in
      apply ~source [ TextEdit.create ~range:edit.range ~newText ]
    in
    assert (String.equal (apply ~source (plain_edits plain)) (expand default));
    Some (expand (Option.value name ~default))
  | _ -> failwith "plain and snippet extraction availability differs"
;;

let test ?name ~kind marked_source =
  let source, range = Test.parse_selection marked_source in
  check ?name ~kind ~source ~range () |> Option.iter ~f:print_string
;;

let%expect_test "local name links without touching existing names or surrounding source" =
  test
    ~kind:`Local
    ~name:"chosen"
    {|let f =
  0 (* var_name stays in this comment *) +
  $(1 + 2)$ + String.length "var_name"
|};
  [%expect
    {|
    let f =
      let chosen = (1 + 2) in
    0 (* var_name stays in this comment *) +
      chosen + String.length "var_name"
    |}]
;;

let%expect_test "function name links and the suffix remains outside the edit" =
  test
    ~kind:`Function
    ~name:"chosen"
    {|let f x =
  (* fun_name stays in this comment *)
  $(x * 2)$ + String.length "fun_name"
|};
  [%expect
    {|
    let chosen x = (x * 2)

    let f x =
      (* fun_name stays in this comment *)
      chosen x + String.length "fun_name"
    |}]
;;

let%expect_test "snippet metacharacters in the body and intervening source stay literal" =
  (* Substitute dollar signs after parsing selection markers; both are one
     UTF-16 code unit, so this does not change the selection's positions. *)
  let source, range =
    Test.parse_selection
      {|let f =
  0 + (* %0 %{1:var_name} %{1:fun_name} \\ } *)
  $(String.length "%0 %{1:var_name} %{1:fun_name} \\ }")$ + 1
|}
  in
  let source = String.tr source ~target:'%' ~replacement:'$' in
  List.iter [ `Local; `Function ] ~f:(fun kind ->
    check ~name:"chosen" ~kind ~source ~range () |> Option.value_exn |> print_endline);
  [%expect
    {|
    let f =
      let chosen = (String.length "$0 ${1:var_name} ${1:fun_name} \\ }") in
    0 + (* $0 ${1:var_name} ${1:fun_name} \\ } *)
      chosen + 1

    let chosen () = (String.length "$0 ${1:var_name} ${1:fun_name} \\ }")

    let f =
      0 + (* $0 ${1:var_name} ${1:fun_name} \\ } *)
      chosen () + 1
    |}]
;;

let%expect_test "nested extraction preserves blank lines, comments, and case suffixes" =
  let source =
    {|let f x =
  match x with
  | None -> 0
  | Some y ->
    let z = y + 1 in

    (* before *)
    $(z * 2)$ + 3 (* after *)
|}
  in
  test ~kind:`Local source;
  test ~kind:`Function source;
  [%expect
    {|
    let f x =
      match x with
      | None -> 0
      | Some y ->
        let z = y + 1 in

        (* before *)
        let var_name = (z * 2) in
    var_name + 3 (* after *)
    let fun_name z = (z * 2)

    let f x =
      match x with
      | None -> 0
      | Some y ->
        let z = y + 1 in

        (* before *)
        fun_name z + 3 (* after *)
    |}]
;;

(* These CR tests deliberately record the existing position-handling bugs.
   Fix them with the general position-encoding work, not the snippet feature. *)
let%expect_test "CR: UTF-16 extraction corrupts source around Unicode" =
  let source = {|let café = String.length "😀" + $String.length "é😀"$ + 1|} in
  test ~kind:`Local source;
  print_newline ();
  test ~kind:`Function source;
  [%expect
    {|
    let café = Slet var_name = t in
    tring.length "😀" + Svar_namering.length "é😀" + 1
    let fun_name () = t

    let café = String.length "😀" + Sfun_name ()ring.length "é😀" + 1
    |}]
;;

let%expect_test
    "CR: UTF-16 extraction selects the wrong expression after a Unicode parameter"
  =
  test ~kind:`Function ~name:"chosen" {|let f café = $café + 1$|};
  [%expect
    {|
    let chosen () = 1

    let f café = café + chosen ()
    |}]
;;

let%expect_test "CR: extraction depends on the negotiated position encoding" =
  let source, range =
    Test.parse_selection {|let café = String.length "😀" + $String.length "é😀"$ + 1|}
  in
  let textDocument =
    TextDocumentItem.create
      ~uri:Helpers.uri
      ~version:0
      ~languageId:(Other "ocaml")
      ~text:source
  in
  let doc =
    Lsp.Text_document.make
      ~position_encoding:`UTF16
      (DidOpenTextDocumentParams.create ~textDocument)
  in
  let start_offset, end_offset = Lsp.Text_document.absolute_range doc range in
  let utf8_doc =
    Lsp.Text_document.make
      ~position_encoding:`UTF8
      (DidOpenTextDocumentParams.create ~textDocument)
  in
  let utf8_range =
    Lsp.Text_document.range_of_utf8_offsets utf8_doc ~start_offset ~end_offset
  in
  let general =
    GeneralClientCapabilities.create ~positionEncodings:[ PositionEncodingKind.UTF8 ] ()
  in
  List.iter [ `Local; `Function ] ~f:(fun kind ->
    let utf16 = check ~kind ~source ~range () |> Option.value_exn in
    let results =
      List.map
        [ ClientCapabilities.create (); Code_actions.snippet_edit_capabilities ]
        ~f:(fun capabilities ->
          let capabilities = { capabilities with general = Some general } in
          let edits =
            request ~capabilities ~kind ~source ~range:utf8_range () |> Option.value_exn
          in
          let edits =
            List.map edits ~f:(function
              | `TextEdit edit -> edit
              | `SnippetTextEdit edit ->
                let name = default_name kind in
                TextEdit.create
                  ~range:edit.range
                  ~newText:(expand ~default:name ~name edit.snippet.value)
              | _ -> assert false)
          in
          apply ~position_encoding:`UTF8 ~source edits)
    in
    match results with
    | [ plain; snippet ] ->
      assert (String.equal plain snippet);
      Printf.printf "UTF-8 matches UTF-16: %b\n" (String.equal utf16 plain);
      print_endline plain
    | _ -> assert false);
  [%expect
    {|
    UTF-8 matches UTF-16: false
    let café = let var_name = String.length "é😀" in
    String.length "😀" + var_name + 1
    UTF-8 matches UTF-16: false
    let fun_name () = String.length "é😀"

    let café = String.length "😀" + fun_name () + 1
    |}]
;;

let%expect_test "CRLF source around the extracted expression is preserved" =
  let source, range =
    Test.parse_selection "let f x =\r\n  (* before *)\r\n  $(x + 1)$ + 2\r\n"
  in
  List.iter [ `Local; `Function ] ~f:(fun kind ->
    let result = check ~kind ~source ~range () |> Option.value_exn in
    assert (String.is_substring result ~substring:"\r\n  (* before *)\r\n");
    assert (String.is_suffix result ~suffix:" + 2\r\n"));
  [%expect {| |}]
;;

let%expect_test
    "workspace capability controls edit kind independently of completion snippets"
  =
  let completionItem = ClientCompletionItemOptions.create ~snippetSupport:true () in
  let completion = CompletionClientCapabilities.create ~completionItem () in
  let textDocument = TextDocumentClientCapabilities.create ~completion () in
  let caps workspace = ClientCapabilities.create ~textDocument ?workspace () in
  let workspace snippetEditSupport =
    let workspaceEdit = WorkspaceEditClientCapabilities.create ?snippetEditSupport () in
    WorkspaceClientCapabilities.create ~workspaceEdit ()
  in
  let source, range = Test.parse_selection {|let f x = $(x * 2)$ + 3|} in
  List.iter [ `Local; `Function ] ~f:(fun kind ->
    let expected =
      request ~capabilities:(ClientCapabilities.create ()) ~kind ~source ~range ()
      |> Option.value_exn
      |> plain_edits
      |> apply ~source
    in
    List.iter
      [ None
      ; Some (WorkspaceClientCapabilities.create ())
      ; Some (workspace None)
      ; Some (workspace (Some false))
      ]
      ~f:(fun workspace ->
        let edits =
          request ~capabilities:(caps workspace) ~kind ~source ~range ()
          |> Option.value_exn
          |> plain_edits
        in
        assert (String.equal expected (apply ~source edits)));
    match
      request ~capabilities:(caps (Some (workspace (Some true)))) ~kind ~source ~range ()
    with
    | Some [ `SnippetTextEdit _ ] -> ()
    | _ -> assert false);
  [%expect {| |}]
;;

let%expect_test "large intervening source is preserved while plain edits stay small" =
  let padding = String.make 100_000 'x' in
  let source, range =
    Test.parse_selection ("let f = 1 + (* " ^ padding ^ " *)\n  $2$ + 3")
  in
  List.iter [ `Local; `Function ] ~f:(fun kind ->
    let result = check ~kind ~source ~range () |> Option.value_exn in
    assert (String.is_substring result ~substring:padding);
    (match
       request
         ~capabilities:Code_actions.snippet_edit_capabilities
         ~kind
         ~source
         ~range
         ()
     with
     | Some [ `SnippetTextEdit edit ] ->
       assert (String.is_substring edit.snippet.value ~substring:padding)
     | _ -> assert false);
    let edits =
      request ~capabilities:(ClientCapabilities.create ()) ~kind ~source ~range ()
      |> Option.value_exn
      |> plain_edits
    in
    assert (
      List.sum
        (module Int)
        edits
        ~f:(fun (edit : TextEdit.t) -> String.length edit.newText)
      < 100));
  [%expect {| |}]
;;

let%expect_test "interfaces and non-expression selections produce no extraction" =
  List.iter [ `Local; `Function ] ~f:(fun kind ->
    List.iter
      [ {|let $value$ = 1|}
      ; {|type t = $int$|}
      ; {|let f = 1 (* $comment$ *)|}
      ; {|let f = function $Some x$ -> x | None -> 0|}
      ]
      ~f:(fun marked ->
        let source, range = Test.parse_selection marked in
        assert (Option.is_none (check ~kind ~source ~range ())));
    let source, range = Test.parse_selection {|val f : $int -> int$|} in
    assert (Option.is_none (check ~path:"foo.mli" ~kind ~source ~range ())));
  [%expect {| |}]
;;
