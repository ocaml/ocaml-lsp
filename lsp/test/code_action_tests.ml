open Lsp.Types

let print only kind =
  print_endline (Bool.to_string (Lsp.Code_action.kind_is_requested only kind))
;;

let%expect_test "code action kind filters" =
  print None CodeActionKind.QuickFix;
  print (Some [ CodeActionKind.Refactor ]) CodeActionKind.RefactorExtract;
  print (Some [ CodeActionKind.Refactor ]) CodeActionKind.QuickFix;
  print
    (Some [ CodeActionKind.Other "ocaml.switch" ])
    (CodeActionKind.Other "ocaml.switch");
  print
    (Some [ CodeActionKind.Other "ocaml.switch" ])
    (CodeActionKind.Other "ocaml.switch.foo");
  print
    (Some [ CodeActionKind.Other "ocaml.switch" ])
    (CodeActionKind.Other "ocaml.switcheroo");
  [%expect
    {|
    true
    true
    false
    true
    true
    false
    |}]
;;
