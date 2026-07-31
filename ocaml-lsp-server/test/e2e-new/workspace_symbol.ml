open Test.Import
open Workspace_symbol_test_helpers

let%expect_test "returns all symbols from workspace" =
  let workspace_a, _workspace_b = setup_workspaces () in
  build_project workspace_a;
  run [ workspace_a ] (fun client ->
    let* symbols = workspace_symbol client "" in
    print_symbols [ workspace_a ] symbols;
    Fiber.return ());
  [%expect
    {|
    stack_of_ints 5 /workspace_symbol_A/bin/a.ml 51:0 65:5
    size 6 /workspace_symbol_A/bin/a.ml 64:4 64:38
    peek 6 /workspace_symbol_A/bin/a.ml 62:4 62:34
    pop 6 /workspace_symbol_A/bin/a.ml 57:4 60:12
    push 6 /workspace_symbol_A/bin/a.ml 55:4 55:45
    the_list 12 /workspace_symbol_A/bin/a.ml 53:4 53:40
    Foo 9 /workspace_symbol_A/bin/a.ml 49:0 49:23
    Increment 2 /workspace_symbol_A/bin/a.ml 45:0 47:3
    increment_x 12 /workspace_symbol_A/bin/a.ml 46:2 46:27
    X_int 2 /workspace_symbol_A/bin/a.ml 41:0 43:3
    x 12 /workspace_symbol_A/bin/a.ml 42:2 42:13
    A_Mod 2 /workspace_symbol_A/bin/a.ml 29:0 39:3
    compare 12 /workspace_symbol_A/bin/a.ml 38:2 38:30
    private_mod_fn 12 /workspace_symbol_A/bin/a.ml 36:2 36:33
    t 15 /workspace_symbol_A/bin/a.ml 34:2 34:14
    My_string 2 /workspace_symbol_A/bin/a.ml 27:0 27:25
    StringMap 2 /workspace_symbol_A/bin/a.ml 26:0 26:36
    a_i 12 /workspace_symbol_A/bin/a.ml 22:0 24:7
    a_arr 12 /workspace_symbol_A/bin/a.ml 18:0 18:14
    a_u 12 /workspace_symbol_A/bin/a.ml 16:0 16:15
    user 15 /workspace_symbol_A/bin/a.ml 12:0 14:12
    NotAdmin 9 /workspace_symbol_A/bin/a.ml 14:2 14:12
    Admin 9 /workspace_symbol_A/bin/a.ml 13:2 13:9
    a_d 12 /workspace_symbol_A/bin/a.ml 7:0 10:14
    A_B 2 /workspace_symbol_A/bin/a.ml 2:0 5:3
    a_b 12 /workspace_symbol_A/bin/a.ml 4:2 4:19
    a_b_t 15 /workspace_symbol_A/bin/a.ml 3:2 3:21
    a_x 12 /workspace_symbol_A/bin/a.ml 0:0 0:11
    main_y 12 /workspace_symbol_A/bin/main.ml 0:0 0:22
    vendored_x 12 /workspace_symbol_A/lib/lib.ml 14:0 14:31
    lib_type 12 /workspace_symbol_A/lib/lib.ml 12:0 12:38
    lib_private_fn 12 /workspace_symbol_A/lib/lib.ml 10:0 10:38
    hd 12 /workspace_symbol_A/lib/lib.ml 8:0 8:16
    lib_x 12 /workspace_symbol_A/lib/lib.ml 6:0 6:14
    user 15 /workspace_symbol_A/lib/lib.ml 3:0 5:1
    name 7 /workspace_symbol_A/lib/lib.ml 4:2 4:14
    t 15 /workspace_symbol_A/lib/LibTypes.mli 0:0 0:15
    x 12 /workspace_symbol_A/vendor/vendored_lib.ml 0:0 0:9
    |}]
;;

let%expect_test "reports deprecated workspace symbols as non-deprecated" =
  let workspace_a, _workspace_b = setup_workspaces () in
  Test.write_file
    (Filename.concat workspace_a.path "lib/lib.mli")
    (a_lib_mli ^ "\nval deprecated_value : int [@@deprecated]\n");
  build_project workspace_a;
  run [ workspace_a ] (fun client ->
    let* symbols = workspace_symbol client "deprecated_value" in
    (match symbols with
     | Some [ symbol ] ->
       let deprecated =
         Option.value_map symbol.deprecated ~default:"missing" ~f:Bool.to_string
       in
       let tags = Option.value_map symbol.tags ~default:"missing" ~f:(fun _ -> "present") in
       Printf.printf
         "name: %s\ndeprecated: %s\ntags: %s\n"
         symbol.name
         deprecated
         tags
     | _ -> print_endline "expected exactly one symbol");
    Fiber.return ());
  [%expect
    {|
    name: deprecated_value
    deprecated: false
    tags: missing
    |}]
;;

let%expect_test "returns filtered symbols from workspace" =
  let workspace_a, _workspace_b = setup_workspaces () in
  build_project workspace_a;
  run [ workspace_a ] (fun client ->
    let* symbols = workspace_symbol client "a_" in
    print_symbols [ workspace_a ] symbols;
    Fiber.return ());
  [%expect
    {|
    a_i 12 /workspace_symbol_A/bin/a.ml 22:0 24:7
    a_arr 12 /workspace_symbol_A/bin/a.ml 18:0 18:14
    a_u 12 /workspace_symbol_A/bin/a.ml 16:0 16:15
    a_d 12 /workspace_symbol_A/bin/a.ml 7:0 10:14
    a_b 12 /workspace_symbol_A/bin/a.ml 4:2 4:19
    a_b_t 15 /workspace_symbol_A/bin/a.ml 3:2 3:21
    a_x 12 /workspace_symbol_A/bin/a.ml 0:0 0:11
    |}]
;;

let%expect_test "handles multiple workspaces" =
  let workspace_a, workspace_b = setup_workspaces () in
  build_project workspace_a;
  build_project workspace_b;
  let workspaces = [ workspace_a; workspace_b ] in
  run workspaces (fun client ->
    let* symbols = workspace_symbol client "" in
    print_symbols workspaces symbols;
    Fiber.return ());
  [%expect
    {|
    stack_of_ints 5 /workspace_symbol_A/bin/a.ml 51:0 65:5
    size 6 /workspace_symbol_A/bin/a.ml 64:4 64:38
    peek 6 /workspace_symbol_A/bin/a.ml 62:4 62:34
    pop 6 /workspace_symbol_A/bin/a.ml 57:4 60:12
    push 6 /workspace_symbol_A/bin/a.ml 55:4 55:45
    the_list 12 /workspace_symbol_A/bin/a.ml 53:4 53:40
    Foo 9 /workspace_symbol_A/bin/a.ml 49:0 49:23
    Increment 2 /workspace_symbol_A/bin/a.ml 45:0 47:3
    increment_x 12 /workspace_symbol_A/bin/a.ml 46:2 46:27
    X_int 2 /workspace_symbol_A/bin/a.ml 41:0 43:3
    x 12 /workspace_symbol_A/bin/a.ml 42:2 42:13
    A_Mod 2 /workspace_symbol_A/bin/a.ml 29:0 39:3
    compare 12 /workspace_symbol_A/bin/a.ml 38:2 38:30
    private_mod_fn 12 /workspace_symbol_A/bin/a.ml 36:2 36:33
    t 15 /workspace_symbol_A/bin/a.ml 34:2 34:14
    My_string 2 /workspace_symbol_A/bin/a.ml 27:0 27:25
    StringMap 2 /workspace_symbol_A/bin/a.ml 26:0 26:36
    a_i 12 /workspace_symbol_A/bin/a.ml 22:0 24:7
    a_arr 12 /workspace_symbol_A/bin/a.ml 18:0 18:14
    a_u 12 /workspace_symbol_A/bin/a.ml 16:0 16:15
    user 15 /workspace_symbol_A/bin/a.ml 12:0 14:12
    NotAdmin 9 /workspace_symbol_A/bin/a.ml 14:2 14:12
    Admin 9 /workspace_symbol_A/bin/a.ml 13:2 13:9
    a_d 12 /workspace_symbol_A/bin/a.ml 7:0 10:14
    A_B 2 /workspace_symbol_A/bin/a.ml 2:0 5:3
    a_b 12 /workspace_symbol_A/bin/a.ml 4:2 4:19
    a_b_t 15 /workspace_symbol_A/bin/a.ml 3:2 3:21
    a_x 12 /workspace_symbol_A/bin/a.ml 0:0 0:11
    main_y 12 /workspace_symbol_A/bin/main.ml 0:0 0:22
    vendored_x 12 /workspace_symbol_A/lib/lib.ml 14:0 14:31
    lib_type 12 /workspace_symbol_A/lib/lib.ml 12:0 12:38
    lib_private_fn 12 /workspace_symbol_A/lib/lib.ml 10:0 10:38
    hd 12 /workspace_symbol_A/lib/lib.ml 8:0 8:16
    lib_x 12 /workspace_symbol_A/lib/lib.ml 6:0 6:14
    user 15 /workspace_symbol_A/lib/lib.ml 3:0 5:1
    name 7 /workspace_symbol_A/lib/lib.ml 4:2 4:14
    t 15 /workspace_symbol_A/lib/LibTypes.mli 0:0 0:15
    x 12 /workspace_symbol_A/vendor/vendored_lib.ml 0:0 0:9
    workspace_B 12 /workspace_symbol_B/main.ml 0:0 0:31
    |}]
;;
