open Test.Import
open Completion

let definitions =
  "type t = Ctor_empty | Ctor_one of int | Ctor_pair of int * bool\n\
   | Ctor_tuple of (int * bool) | Ctor_record of { x : int; y : bool }\n\
   | Ctor_fn of (int -> int)\n"
;;

let iter ?path ?(capabilities = snippet_capabilities) source f =
  let source, position = Test.parse_cursor source in
  iter_completions ?path ~capabilities ~source ~position (function
    | Some (`CompletionList { isIncomplete; items; _ }) ->
      assert (not isIncomplete);
      List.iteri items ~f:(fun index (item : CompletionItem.t) ->
        assert (item.sortText = Some (Printf.sprintf "%04d" index)));
      f source items
    | _ -> failwith "expected a complete completion list")
;;

let apply source (item : CompletionItem.t) =
  let edit =
    match item.textEdit with
    | Some (`TextEdit edit) -> edit
    | _ -> failwith "expected a text edit"
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
  Lsp.Text_document.apply_text_document_edits
    doc
    (edit :: Option.value item.additionalTextEdits ~default:[])
  |> Lsp.Text_document.text
;;

let%expect_test "constructor payload shapes retain ordinary completions" =
  iter (definitions ^ "let value = Ctor$") (fun _ items ->
    List.iter items ~f:(fun (item : CompletionItem.t) ->
      if String.is_prefix item.label ~prefix:"Ctor"
      then (
        let text =
          match item.textEdit with
          | Some (`TextEdit edit) -> edit.newText
          | _ -> failwith "expected a text edit"
        in
        Printf.printf "%s: %s => %s\n" item.label (Option.value_exn item.detail) text)));
  [%expect
    {|
    Ctor_empty: t => Ctor_empty
    Ctor_fn: (int -> int) -> t => Ctor_fn
    Ctor_one: int -> t => Ctor_one
    Ctor_pair: int * bool -> t => Ctor_pair
    Ctor_record: t.Ctor_record -> t => Ctor_record
    Ctor_tuple: (int * bool) -> t => Ctor_tuple
    Ctor_fn (call): (int -> int) -> t => (Ctor_fn ${1:_})$0
    Ctor_one (call): int -> t => (Ctor_one ${1:_})$0
    Ctor_pair (call): int * bool -> t => (Ctor_pair (${1:_}, ${2:_}))$0
    Ctor_record (call): t.Ctor_record -> t => (Ctor_record ${1:_})$0
    Ctor_tuple (call): (int * bool) -> t => (Ctor_tuple ${1:_})$0
    |}]
;;

let%expect_test "constructor calls in function arguments and qualified paths" =
  List.iter
    [ "type t = Ctor of int\nlet consume (_ : t) = ()\nlet _ = consume Ct$"
    ; "module M = struct type t = Ctor of int end\nlet _ = M.Ct$"
    ; "module M = struct type t = Ctor of int end\nlet _ = M . Ct$"
    ; "type t = Ctor of int\nlet f = function Ct$ -> ()"
    ]
    ~f:(fun source ->
      iter source (fun source items ->
        List.iter items ~f:(fun (item : CompletionItem.t) ->
          if String.equal item.label "Ctor (call)" then print_endline (apply source item))));
  [%expect
    {|
    type t = Ctor of int
    let consume (_ : t) = ()
    let _ = consume (Ctor ${1:_})$0
    module M = struct type t = Ctor of int end
    let _ = (M.Ctor ${1:_})$0
    module M = struct type t = Ctor of int end
    let _ = (M . Ctor ${1:_})$0
    type t = Ctor of int
    let f = function (Ctor ${1:_})$0 -> ()
    |}]
;;

let%expect_test "constructor snippet capabilities and ordering" =
  List.iter [ None; Some false; Some true ] ~f:(fun snippetSupport ->
    let completionItem = ClientCompletionItemOptions.create ?snippetSupport () in
    let completion = CompletionClientCapabilities.create ~completionItem () in
    let textDocument = TextDocumentClientCapabilities.create ~completion () in
    let capabilities = ClientCapabilities.create ~textDocument () in
    iter
      ~capabilities
      "type t = Ctor_a of int | Ctor_b | Ctor_c of bool\nlet value = Ctor$"
      (fun _ items ->
         Printf.printf
           "%s: %s\n"
           (Option.value_map snippetSupport ~default:"absent" ~f:string_of_bool)
           (List.filter_map items ~f:(fun (item : CompletionItem.t) ->
              Option.some_if (String.is_prefix item.label ~prefix:"Ctor") item.label)
            |> String.concat ~sep:", ")));
  [%expect
    {|
    absent: Ctor_a, Ctor_b, Ctor_c
    false: Ctor_a, Ctor_b, Ctor_c
    true: Ctor_a, Ctor_b, Ctor_c, Ctor_a (call), Ctor_c (call)
    |}]
;;

let%expect_test "workspace snippet support alone does not enable constructor completions" =
  let capabilities = Code_actions.snippet_edit_capabilities in
  iter ~capabilities "type t = Ctor of int\nlet value = Ct$" (fun _ items ->
    assert (List.exists items ~f:(fun (item : CompletionItem.t) -> item.label = "Ctor"));
    assert (
      not
        (List.exists items ~f:(fun (item : CompletionItem.t) -> item.kind = Some Snippet))));
  [%expect {| |}]
;;

let%expect_test "constructor completion replaces suffixes" =
  List.iter [ "Ct$"; "Ct$or_typo"; "Ctor$" ] ~f:(fun prefix ->
    iter ("type t = Ctor of int\nlet value : t = " ^ prefix) (fun source items ->
      let item =
        List.find_exn items ~f:(fun (item : CompletionItem.t) ->
          item.label = "Ctor (call)")
      in
      print_endline (apply source item)));
  [%expect
    {|
    type t = Ctor of int
    let value : t = (Ctor ${1:_})$0
    type t = Ctor of int
    let value : t = (Ctor ${1:_})$0
    type t = Ctor of int
    let value : t = (Ctor ${1:_})$0
    |}]
;;

(* CR: These position bugs are shared with ordinary completions. Fix them with
   the general position-encoding work, not the constructor snippet feature. *)
let%expect_test "CR: constructor completion after Unicode uses the wrong prefix" =
  List.iter [ "Ct$"; "Ct$or_typo"; "Ctor$" ] ~f:(fun prefix ->
    iter
      ("type t = Ctor of int\nlet café : t = let _ = \"😀\" in " ^ prefix)
      (fun source items ->
         let items =
           List.filter items ~f:(fun (item : CompletionItem.t) ->
             String.is_prefix item.label ~prefix:"Ctor")
         in
         Printf.printf "%s: %d constructor completions\n" prefix (List.length items);
         List.iter items ~f:(fun item ->
           print_endline (List.last_exn (String.split_lines (apply source item))))));
  [%expect
    {|
    Ct$: 0 constructor completions
    Ct$or_typo: 0 constructor completions
    Ctor$: 1 constructor completions
    let café : t = let _ = "😀" in CtoCtor
    |}]
;;

let%expect_test "constructor names may contain Unicode" =
  iter "type t = Café of int\nlet value = Caf$" (fun source items ->
    let item =
      List.find_exn items ~f:(fun (item : CompletionItem.t) -> item.label = "Café (call)")
    in
    print_endline (apply source item));
  [%expect
    {|
    type t = Café of int
    let value = (Café ${1:_})$0
    |}]
;;

let%expect_test "exception and GADT constructors with payloads" =
  List.iter
    [ "exception Ctor of string\nlet value = Ct$"
    ; "type _ t = Ctor : int -> int t\nlet value = Ct$"
    ]
    ~f:(fun source ->
      iter source (fun source items ->
        let item =
          List.find_exn items ~f:(fun (item : CompletionItem.t) ->
            item.label = "Ctor (call)")
        in
        print_endline (apply source item)));
  [%expect
    {|
    exception Ctor of string
    let value = (Ctor ${1:_})$0
    type _ t = Ctor : int -> int t
    let value = (Ctor ${1:_})$0
    |}]
;;

let%expect_test "constructor snippets preserve metadata but disable symbol resolution" =
  List.iter [ false; true ] ~f:(fun tags ->
    let tagSupport =
      Option.some_if tags (CompletionItemTagOptions.create ~valueSet:[ Deprecated ])
    in
    let resolveSupport =
      ClientCompletionItemResolveOptions.create ~properties:[ "documentation" ]
    in
    let completionItem =
      ClientCompletionItemOptions.create
        ~snippetSupport:true
        ~deprecatedSupport:true
        ?tagSupport
        ~resolveSupport
        ()
    in
    let completionItemKind =
      ClientCompletionItemOptionsKind.create
        ~valueSet:[ Constructor; EnumMember; Snippet ]
        ()
    in
    let completion =
      CompletionClientCapabilities.create ~completionItem ~completionItemKind ()
    in
    let textDocument = TextDocumentClientCapabilities.create ~completion () in
    let capabilities = ClientCapabilities.create ~textDocument () in
    iter
      ~capabilities
      "module M : sig type t = Ctor of int [@deprecated] end = struct type t = Ctor of \
       int end\n\
       let value = M.Ct$"
      (fun _ items ->
         let find label =
           List.find_exn items ~f:(fun (item : CompletionItem.t) -> item.label = label)
         in
         let plain = find "Ctor"
         and call = find "Ctor (call)" in
         assert (plain.kind = Some Constructor);
         assert (call.kind = Some Snippet && call.insertTextFormat = Some Snippet);
         assert (Option.is_some plain.data && Option.is_none call.data);
         assert (
           call.detail = plain.detail
           && call.deprecated = plain.deprecated
           && call.tags = plain.tags);
         assert (call.filterText = Some "Ctor");
         assert (Option.is_none call.command);
         assert (call.insertTextMode = None);
         (match call.additionalTextEdits with
          | Some [ edit ] -> assert (String.is_empty edit.newText)
          | _ -> assert false);
         assert (
           if tags then call.tags = Some [ Deprecated ] else call.deprecated = Some true)));
  [%expect {| |}]
;;

let%expect_test "constructor calls are disabled outside OCaml" =
  let source, position = Test.parse_cursor "type t = Ctor of int\nlet value = Ct$" in
  List.iter [ "reason"; "ocaml.mlx" ] ~f:(fun language_id ->
    Helpers.test ~language_id ~capabilities:snippet_capabilities source (fun client ->
      let+ response = request_completions client position in
      let items =
        match Option.value_exn response with
        | `CompletionList list -> list.items
        | `List items -> items
      in
      assert (List.exists items ~f:(fun (item : CompletionItem.t) -> item.label = "Ctor"));
      assert (
        not
          (List.exists items ~f:(fun (item : CompletionItem.t) ->
             item.kind = Some Snippet)))));
  [%expect {| |}]
;;

let%expect_test "constructor declarations stay name-only" =
  List.iter [ "test.ml"; "test.mli" ] ~f:(fun path ->
    List.iter
      [ "type t = Ctor of int\ntype u = Ct$"
      ; "type t = Ctor of int\ntype u = A | Ct$"
      ; "type t = Ctor of int\ntype u = ..\ntype u += Ct$"
      ; "exception Ctor of int\nexception Ct$"
      ]
      ~f:(fun source ->
        iter ~path source (fun _ items ->
          if
            not
              (List.exists items ~f:(fun (item : CompletionItem.t) -> item.label = "Ctor"))
          then failwith (path ^ ": " ^ source);
          assert (
            not
              (List.exists items ~f:(fun (item : CompletionItem.t) ->
                 item.label = "Ctor (call)"))))));
  [%expect {| |}]
;;

let%expect_test "qualified constructor calls preserve comments and payload scope" =
  List.iter
    [ "M.(* comment *)Ct$"
    ; "M (* outer (* nested *) *). Ct$"
    ; "M. (* \"quoted\" *) Ct$or_typo"
    ]
    ~f:(fun reference ->
      iter
        ("module M = struct type t = Ctor of int let payload = false end\n\
          open M\n\
          let payload = 42\n\
          let value = "
         ^ reference)
        (fun source items ->
           let item =
             List.find_exn items ~f:(fun (item : CompletionItem.t) ->
               item.label = "Ctor (call)")
           in
           let source = apply source item in
           let source =
             String.substr_replace_all source ~pattern:"${1:_}" ~with_:"payload"
           in
           let source = String.substr_replace_all source ~pattern:"$0" ~with_:"" in
           print_endline (List.last_exn (String.split_lines source))));
  [%expect
    {|
    let value = (M.(* comment *)Ctor payload)
    let value = (M (* outer (* nested *) *). Ctor payload)
    let value = (M. (* "quoted" *) Ctor payload)
    |}]
;;

let%expect_test "constructor reference contexts include empty prefixes and patterns" =
  List.iter
    [ "let value = M.$"
    ; "let value = Ct$"
    ; "let f = function M.Ct$ -> ()"
    ; "let f = function M.(* comment *)Ct$ -> ()"
    ; "let f = function Ct$ -> ()"
    ; "let f (M.Ct$) = ()"
    ; "let value = M.(Ct$)"
    ]
    ~f:(fun expression ->
      iter
        ("module M = struct type t = Ctor of int end\nopen M\n" ^ expression)
        (fun source items ->
           let item =
             List.find_exn items ~f:(fun (item : CompletionItem.t) ->
               item.label = "Ctor (call)")
           in
           print_endline (List.last_exn (String.split_lines (apply source item)))));
  [%expect
    {|
    let value = (M.Ctor ${1:_})$0
    let value = (Ctor ${1:_})$0
    let f = function (M.Ctor ${1:_})$0 -> ()
    let f = function (M.(* comment *)Ctor ${1:_})$0 -> ()
    let f = function (Ctor ${1:_})$0 -> ()
    let f ((M.Ctor ${1:_})$0) = ()
    let value = M.((Ctor ${1:_})$0)
    |}]
;;

let%expect_test "multiline qualified paths and polymorphic variants stay name-only" =
  List.iter
    [ "module M = struct type t = Ctor of int end\nlet value = M.\nCt$"
    ; "module M = struct type t = Ctor of int end\n\
       open M\n\
       let value = M.(*\n\
       comment *)Ct$"
    ; "type t = [ `Ctor of int ]\nlet value : t = `Ct$"
    ]
    ~f:(fun source ->
      iter source (fun _ items ->
        assert (not (List.is_empty items));
        assert (
          not
            (List.exists items ~f:(fun (item : CompletionItem.t) ->
               item.kind = Some Snippet)))));
  [%expect {| |}]
;;
