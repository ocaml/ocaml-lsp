open Test.Import

let run_test ~title ~message source =
  let src, range = Code_actions.parse_selection source in
  Code_actions.apply_code_action
    ~diagnostics:[ Diagnostic.create ~message:(`String message) ~range () ]
    title
    src
    range
  |> Option.iter ~f:print_string
;;

let mark_test = function
  | `Value -> run_test ~title:"Mark as unused" ~message:"Warning 26: unused variable x."
  | `Open ->
    run_test
      ~title:"Replace with open!"
      ~message:"Error (warning 33 [unused-open]): unused open B."
  | `For_loop_index ->
    run_test
      ~title:"Mark for-loop index as unused"
      ~message:"Warning 35: unused for-loop index i."
;;

let remove_test = function
  | `Value -> run_test ~title:"Remove unused" ~message:"Warning 26: unused variable x."
  | `Open -> run_test ~title:"Remove unused open" ~message:"Warning 33: unused open B."
  | `Open_bang ->
    run_test ~title:"Remove unused open!" ~message:"Warning 66: unused open! B."
  | `Type -> run_test ~title:"Remove unused type" ~message:"Warning 34: unused type t."
  | `Module ->
    run_test ~title:"Remove unused module" ~message:"Warning 60: unused module M."
  | `Case ->
    run_test ~title:"Remove unused case" ~message:"Warning 11: this match case is unused."
  | `Rec -> run_test ~title:"Remove unused rec" ~message:"Warning 39: unused rec flag."
  | `Constructor ->
    run_test
      ~title:"Remove unused constructor"
      ~message:"Warning 37: unused constructor A."
;;

let%expect_test "mark value in let" =
  mark_test
    `Value
    {|
let f =
  let $x$ = 1 in
  0
|};
  [%expect {| |}]
;;

(* todo *)
let%expect_test "mark value in top level let" =
  mark_test
    `Value
    {|
let $f$ =
  let x = 1 in
  0
|};
  [%expect {| |}]
;;

let%expect_test "mark value in match" =
  mark_test
    `Value
    {|
let f = function
  | $x$ -> 0
|};
  [%expect {| |}]
;;

let%expect_test "remove value in let" =
  remove_test
    `Value
    {|
let f =
  let $x$ = 1 in
  0
|};
  [%expect {| |}]
;;

(* todo *)
let%expect_test "remove value in top level let" =
  remove_test
    `Value
    {|
let $f$ =
  let x = 1 in
  0
|}
;;

let%expect_test "mark open" =
  mark_test
    `Open
    {|
$open M$
|};
  [%expect {| |}]
;;

let%expect_test "mark for loop index" =
  mark_test
    `For_loop_index
    {|
let () =
  for $i$ = 0 to 10 do
    ()
  done
|};
  [%expect {| |}]
;;

let%expect_test "remove open" =
  remove_test
    `Open
    {|
open A
$open B$
|};
  [%expect {| |}]
;;

let%expect_test "remove open!" =
  remove_test
    `Open_bang
    {|
open A
$open! B$
|}
;;

let%expect_test "remove type" =
  remove_test
    `Type
    {|
$type t = int$
type s = bool
|};
  [%expect {| |}]
;;

let%expect_test "remove module" =
  remove_test
    `Module
    {|
$module A = struct end$
module B = struct end
|};
  [%expect {| |}]
;;

let%expect_test "remove case" =
  remove_test
    `Case
    {|
let f = function
 | 0 -> 0
 | $0 -> 1$
|};
  [%expect {| |}]
;;

let%expect_test "remove case after Unicode" =
  let source, range =
    Code_actions.parse_selection
      {|
let f = function
 | 0 -> 0
 | $"😀" -> 1$
|}
  in
  let diagnostics =
    [ Diagnostic.create
        ~message:(`String "Warning 11: this match case is unused.")
        ~range
        ()
    ]
  in
  Code_actions.print_code_actions
    ~diagnostics
    ~filter:(function
      | `CodeAction { title; _ } -> String.equal title "Remove unused case"
      | `Command _ -> false)
    source
    range;
  [%expect
    {| No code actions |}]
;;

let%expect_test "remove rec flag after Unicode" =
  let source, range =
    Code_actions.parse_selection
      {|
let café = let rec $f$ = 0 in f
|}
  in
  let diagnostics =
    [ Diagnostic.create ~message:(`String "Warning 39: unused rec flag.") ~range () ]
  in
  Code_actions.print_code_actions
    ~diagnostics
    ~filter:(function
      | `CodeAction { title; _ } -> String.equal title "Remove unused rec"
      | `Command _ -> false)
    source
    range;
  [%expect
    {| No code actions |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t = A $| B$
|};
  [%expect {| |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t =
  | A
 $| B$
|};
  [%expect {| |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t =
 $| A$
 | B
|};
  [%expect {| |}]
;;

let%expect_test "remove constructor" =
  remove_test
    `Constructor
    {|
type t =
 $A$
 | B
|};
  [%expect {| |}]
;;

let%expect_test "deprecated alert text is not an unused diagnostic" =
  let source =
    {ocaml|module M = struct let x = 1 end [@@deprecated "do not use: unused open"]
open M
let y = x
|ocaml}
  in
  let published_diagnostics = Fiber.Ivar.create () in
  let handler =
    Client.Handler.make
      ~on_notification:(fun _ -> function
         | PublishDiagnostics diagnostics ->
           let* filled = Fiber.Ivar.peek published_diagnostics in
           (match filled with
            | Some _ -> Fiber.return ()
            | None -> Fiber.Ivar.fill published_diagnostics diagnostics)
         | _ -> Fiber.return ())
      ()
  in
  Test.run_initialized ~handler (fun client ->
    let* () = Test.open_document ~client ~uri:Helpers.uri ~source () in
    let* { PublishDiagnosticsParams.diagnostics; _ } =
      Fiber.Ivar.read published_diagnostics
    in
    let diagnostic =
      List.find_exn diagnostics ~f:(fun (diagnostic : Diagnostic.t) ->
        let message =
          match diagnostic.message with
          | `String message -> message
          | `MarkupContent { value; _ } -> value
        in
        String.is_prefix message ~prefix:"Alert deprecated")
    in
    let textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri in
    let context =
      CodeActionContext.create
        ~diagnostics:[ diagnostic ]
        ~only:[ CodeActionKind.QuickFix ]
        ()
    in
    let params =
      CodeActionParams.create ~textDocument ~range:diagnostic.range ~context ()
    in
    let* response = Client.request client (CodeAction params) in
    Code_actions.print_code_action_result
      ~filter:(function
        | `CodeAction { title; _ } -> String.equal title "Remove unused open"
        | `Command _ -> false)
      response;
    Test.shutdown_client client);
  [%expect {| No code actions |}]
;;
