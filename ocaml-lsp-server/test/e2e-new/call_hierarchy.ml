open Async
open Test.Import

let call_hierachy_prepare client position =
  Client.request
    client
    (TextDocumentPrepareCallHierarchy
       { CallHierarchyPrepareParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri:Helpers.uri
       ; workDoneToken = None
       })
;;

let call_hierarchy_incoming client item =
  Client.request
    client
    (CallHierarchyIncomingCalls (CallHierarchyIncomingCallsParams.create ~item ()))
;;

let print_selection_range
  ?(indent_spaces = 0)
  ?(sourrounding_lines = 1)
  source
  ({ start; end_ } : Range.t)
  =
  let lines = String.split_lines source in
  List.fold_left lines ~init:(0, []) ~f:(fun (current_line, lines) line ->
    let new_lines =
      if start.line <= current_line && current_line <= end_.line
      then (
        let make = String.make in
        let line_length = String.length line in
        let start_c, end_c = start.character, end_.character in
        let markers =
          match start.line = current_line, end_.line = current_line with
          | true, false -> make start_c ' ' ^ make (line_length - start_c) '^'
          | false, true -> make end_c '^'
          | true, true -> make start_c ' ' ^ make (end_c - start_c) '^'
          | false, false -> make line_length '^'
        in
        [ markers; line ])
      else if start.line - sourrounding_lines <= current_line
              && current_line <= end_.line + sourrounding_lines
      then [ line ]
      else []
    in
    current_line + 1, new_lines @ lines)
  |> snd
  |> List.rev
  |> List.map ~f:(fun line -> String.make indent_spaces ' ' ^ line)
  |> String.concat ~sep:"\n"
  |> print_endline
;;

let%expect_test "print selection range" =
  let source =
    {ocaml|
let main () =
  let baz b =
    let a = Bar.bar b in
    Foo.foo a
  in
baz 5
;;
|ocaml}
    |> String.trim
  in
  let start = Position.create ~line:1 ~character:2 in
  let end_ = Position.create ~line:4 ~character:3 in
  print_selection_range source (Range.create ~start ~end_);
  [%expect
    {|
    let main () =
      let baz b =
      ^^^^^^^^^^^
        let a = Bar.bar b in
    ^^^^^^^^^^^^^^^^^^^^^^^^
        Foo.foo a
    ^^^^^^^^^^^^^
      in
    ^^^
    baz 5
    |}];
  let start = Position.create ~line:2 ~character:12 in
  let end_ = Position.create ~line:2 ~character:19 in
  print_selection_range source (Range.create ~start ~end_);
  [%expect
    {|
    let baz b =
      let a = Bar.bar b in
              ^^^^^^^
      Foo.foo a
    |}];
  return ()
;;

let print_prepare ?(details = true) prepare source =
  match prepare with
  | None -> print_endline "no prepare response"
  | Some prepare ->
    List.iter prepare ~f:(fun (item : CallHierarchyItem.t) ->
      if details
      then (
        print_endline "Prepare Item:";
        print_endline ("  Detail: " ^ Option.value item.detail ~default:"");
        print_endline ("  Name:   " ^ item.name);
        print_endline ("  Uri:    " ^ Uri.to_string item.uri));
      print_endline "  Range:";
      print_selection_range ~indent_spaces:4 ~sourrounding_lines:0 source item.range;
      print_endline "  Selection Range:";
      print_selection_range
        ~indent_spaces:4
        ~sourrounding_lines:0
        source
        item.selectionRange)
;;

let print_incoming incoming =
  match incoming with
  | None -> print_endline "no incoming response"
  | Some incoming ->
    List.iter incoming ~f:(fun i ->
      CallHierarchyIncomingCall.yojson_of_t i
      |> Yojson.Safe.pretty_to_string ~std:false
      |> print_endline)
;;

let print_cursor_position source (position : Position.t) =
  let range =
    Range.create
      ~start:position
      ~end_:{ position with character = position.character + 1 }
  in
  print_selection_range ~sourrounding_lines:0 source range
;;

let call_stack_tree_req client position source =
  let format_line name detail line =
    name
    ^ " -- "
    ^ (Option.map detail ~f:(fun d -> d ^ " @ " ^ Int.to_string line)
       |> Option.value ~default:"")
  in
  let format_incoming
    { CallHierarchyIncomingCall.from =
        { detail; name; range = { start = { line; _ }; _ }; _ }
    ; _
    }
    =
    format_line name detail line
  in
  let rec resolve item =
    let* resp = call_hierarchy_incoming client item in
    match resp with
    | None -> Fiber.return None
    | Some items ->
      let fibers =
        List.map items ~f:(fun (incoming_item : CallHierarchyIncomingCall.t) ->
          let+ from = resolve incoming_item.from in
          match from with
          | None | Some [] -> Expectree.Leaf (format_incoming incoming_item)
          | Some nodes -> Expectree.Branch (format_incoming incoming_item, nodes))
      in
      let+ fibers = Fiber.all fibers in
      Some fibers
  in
  let* resp = call_hierachy_prepare client position in
  print_prepare ~details:false resp source;
  let ({ range = { start = { line; _ }; _ }; _ } as prepare_item) : CallHierarchyItem.t =
    resp |> Option.value_exn |> List.hd
  in
  let+ tree = resolve prepare_item in
  let tree =
    Expectree.Branch
      (format_line prepare_item.name prepare_item.detail line, Option.value_exn tree)
  in
  Expectree.to_string tree |> print_endline
;;

let test_prepare source =
  let source = String.trim source in
  let test ~line ~character =
    let position = Position.create ~line ~character in
    print_cursor_position source position;
    let req client =
      let+ resp = call_hierachy_prepare client position in
      print_prepare resp source
    in
    Helpers.test source req
  in
  test
;;

let test_incoming source =
  let source = source |> String.trim in
  let test ~line ~character =
    let position = Position.create ~line ~character in
    print_cursor_position source position;
    print_newline ();
    let req client = call_stack_tree_req client position source in
    Helpers.test source req
  in
  test
;;

let%expect_test "handle prepare" =
  let source =
    {ocaml|
module Foo = struct
  let foo a = a + 1
end

module Bar = struct
  let bar = fun a -> Foo.foo a + 1
end

let main () =
  let baz b =
    let a = Bar.bar b in
    Foo.foo a
  in
  baz 5
;;
|ocaml}
  in
  let test = test_prepare source in
  (* Variable a is not a function so we expect no response *)
  let%bind () = test ~line:10 ~character:8 in
  [%expect
    {|
        let a = Bar.bar b in
            ^
    no prepare response
    |}];
  (* Resolve baz as you are on the function defintion *)
  let%bind () = test ~line:9 ~character:6 in
  [%expect
    {|
      let baz b =
          ^
    Prepare Item:
      Detail: test.ml
      Name:   baz
      Uri:    file:///test.ml
      Range:
          let baz b =
              ^^^
      Selection Range:
          let baz b =
              ^^^
    |}];
  (* Resolve Foo.foo as you are on the function defintion *)
  let%bind () = test ~line:1 ~character:7 in
  [%expect
    {|
      let foo a = a + 1
           ^
    Prepare Item:
      Detail: test.ml
      Name:   Foo.foo
      Uri:    file:///test.ml
      Range:
          let foo a = a + 1
              ^^^
      Selection Range:
          let foo a = a + 1
              ^^^
    |}];
  (* We expect to resolve the function baz defined in main *)
  let%bind () = test ~line:13 ~character:2 in
  [%expect
    {|
      baz 5
      ^
    Prepare Item:
      Detail: test.ml
      Name:   baz
      Uri:    file:///test.ml
      Range:
          let baz b =
              ^^^
      Selection Range:
          let baz b =
              ^^^
    |}];
  (* We expect to resolve the function foo in the Foo module *)
  let%bind () = test ~line:11 ~character:9 in
  [%expect
    {|
        Foo.foo a
             ^
    Prepare Item:
      Detail: test.ml
      Name:   Foo.foo
      Uri:    file:///test.ml
      Range:
          let foo a = a + 1
              ^^^
      Selection Range:
          let foo a = a + 1
              ^^^
    |}];
  (* We Expect to resolve the function bar in the Bar module *)
  let%bind () = test ~line:10 ~character:17 in
  [%expect
    {|
        let a = Bar.bar b in
                     ^
    Prepare Item:
      Detail: test.ml
      Name:   Bar.bar
      Uri:    file:///test.ml
      Range:
          let bar = fun a -> Foo.foo a + 1
              ^^^
      Selection Range:
          let bar = fun a -> Foo.foo a + 1
              ^^^
    |}];
  return ()
;;

let%expect_test "incoming with modules" =
  let source =
    {ocaml|
module Foo = struct
  module Inner_foo = struct
    let foo a = a + 1
  end
  let foo a = Inner_foo.foo a + 1
  let foo' = Inner_foo.foo
end

module Bar = struct
  let bar = fun a -> Foo.foo a + 1
end

let main () =
  let baz b =
    let a = Bar.bar b in
    Foo.foo' a
  in
  baz 5
;;
  |ocaml}
  in
  let test = test_incoming source in
  let%bind () = test ~line:2 ~character:8 in
  [%expect
    {|
        let foo a = a + 1
            ^

      Range:
            let foo a = a + 1
                ^^^
      Selection Range:
            let foo a = a + 1
                ^^^
    Foo.Inner_foo.foo -- test.ml @ 2
    ├─╴Foo.foo -- test.ml @ 4
    │  ╰─╴Bar.bar -- test.ml @ 9
    │     ╰─╴baz -- test.ml @ 13
    │        ╰─╴main -- test.ml @ 12
    ╰─╴Foo.foo' -- test.ml @ 5
       ╰─╴baz -- test.ml @ 13
          ╰─╴main -- test.ml @ 12
    |}];
  let%bind () = test ~line:5 ~character:24 in
  [%expect
    {|
      let foo' = Inner_foo.foo
                            ^

      Range:
            let foo a = a + 1
                ^^^
      Selection Range:
            let foo a = a + 1
                ^^^
    Foo.Inner_foo.foo -- test.ml @ 2
    ├─╴Foo.foo -- test.ml @ 4
    │  ╰─╴Bar.bar -- test.ml @ 9
    │     ╰─╴baz -- test.ml @ 13
    │        ╰─╴main -- test.ml @ 12
    ╰─╴Foo.foo' -- test.ml @ 5
       ╰─╴baz -- test.ml @ 13
          ╰─╴main -- test.ml @ 12
    |}];
  let%bind () = test ~line:5 ~character:6 in
  [%expect
    {|
      let foo' = Inner_foo.foo
          ^

      Range:
          let foo' = Inner_foo.foo
              ^^^^
      Selection Range:
          let foo' = Inner_foo.foo
              ^^^^
    Foo.foo' -- test.ml @ 5
    ╰─╴baz -- test.ml @ 13
       ╰─╴main -- test.ml @ 12
    |}];
  let%bind () = test ~line:14 ~character:18 in
  [%expect
    {|
        let a = Bar.bar b in
                      ^

      Range:
          let bar = fun a -> Foo.foo a + 1
              ^^^
      Selection Range:
          let bar = fun a -> Foo.foo a + 1
              ^^^
    Bar.bar -- test.ml @ 9
    ╰─╴baz -- test.ml @ 13
       ╰─╴main -- test.ml @ 12
    |}];
  let%bind () = test ~line:15 ~character:9 in
  [%expect
    {|
        Foo.foo' a
             ^

      Range:
          let foo' = Inner_foo.foo
              ^^^^
      Selection Range:
          let foo' = Inner_foo.foo
              ^^^^
    Foo.foo' -- test.ml @ 5
    ╰─╴baz -- test.ml @ 13
       ╰─╴main -- test.ml @ 12
    |}];
  return ()
;;

let%expect_test "incoming" =
  let source =
    {ocaml|
let foo1 a = a + 1;;
let foo2 a = foo1 a + 1;;
let foo3 a = foo2 a + 1;;
let foo4 a = foo3 a + 1;;
let foo2_3 a = foo2 a + foo3 a;;
  |ocaml}
    |> String.trim
  in
  let test = test_incoming source in
  let%bind () = test ~line:1 ~character:14 in
  [%expect
    {|
    let foo2 a = foo1 a + 1;;
                  ^

      Range:
        let foo1 a = a + 1;;
            ^^^^
      Selection Range:
        let foo1 a = a + 1;;
            ^^^^
    foo1 -- test.ml @ 0
    ╰─╴foo2 -- test.ml @ 1
       ├─╴foo3 -- test.ml @ 2
       │  ├─╴foo4 -- test.ml @ 3
       │  ╰─╴foo2_3 -- test.ml @ 4
       ╰─╴foo2_3 -- test.ml @ 4
    |}];
  let%bind () = test ~line:0 ~character:5 in
  [%expect
    {|
    let foo1 a = a + 1;;
         ^

      Range:
        let foo1 a = a + 1;;
            ^^^^
      Selection Range:
        let foo1 a = a + 1;;
            ^^^^
    foo1 -- test.ml @ 0
    ╰─╴foo2 -- test.ml @ 1
       ├─╴foo3 -- test.ml @ 2
       │  ├─╴foo4 -- test.ml @ 3
       │  ╰─╴foo2_3 -- test.ml @ 4
       ╰─╴foo2_3 -- test.ml @ 4
    |}];
  return ()
;;

let%expect_test "multiple calls of function in the same parent function" =
  let source =
    {ocaml|
let a a =
  let b b =
    let c c = c + 1 in
    c b + c a
  in
  b (a + 1)
;;
|ocaml}
  in
  let test = test_incoming source in
  let%bind () = test ~line:3 ~character:10 in
  [%expect
    {|
        c b + c a
              ^

      Range:
            let c c = c + 1 in
                ^
      Selection Range:
            let c c = c + 1 in
                ^
    c -- test.ml @ 2
    ╰─╴b -- test.ml @ 1
       ╰─╴a -- test.ml @ 0
    |}];
  return ()
;;

let%expect_test "works with type constraints" =
  let source =
    {ocaml|
let f x : int = String.length x
let c : int = f "foo" + f "bar"
let g x : int = f x + c
let h x y: int * int = g x, f y
;;
|ocaml}
  in
  let test = test_incoming source in
  let%bind () = test ~line:0 ~character:4 in
  [%expect
    {|
    let f x : int = String.length x
        ^

      Range:
        let f x : int = String.length x
            ^
      Selection Range:
        let f x : int = String.length x
            ^
    f -- test.ml @ 0
    ├─╴c -- test.ml @ 1
    │  ╰─╴g -- test.ml @ 2
    │     ╰─╴h -- test.ml @ 3
    ├─╴g -- test.ml @ 2
    │  ╰─╴h -- test.ml @ 3
    ╰─╴h -- test.ml @ 3
    |}];
  let%bind () = test ~line:1 ~character:14 in
  [%expect
    {|
    let c : int = f "foo" + f "bar"
                  ^

      Range:
        let f x : int = String.length x
            ^
      Selection Range:
        let f x : int = String.length x
            ^
    f -- test.ml @ 0
    ├─╴c -- test.ml @ 1
    │  ╰─╴g -- test.ml @ 2
    │     ╰─╴h -- test.ml @ 3
    ├─╴g -- test.ml @ 2
    │  ╰─╴h -- test.ml @ 3
    ╰─╴h -- test.ml @ 3
    |}];
  return ()
;;

let%expect_test "aliases" =
  let source =
    {ocaml|
let f x = x + 1
let g = f
let h = g
let foo = h 5
let bar = g 7
let baz = f 2
;;
|ocaml}
  in
  let test = test_incoming source in
  let%bind () = test ~line:1 ~character:8 in
  [%expect
    {|
    let g = f
            ^

      Range:
        let f x = x + 1
            ^
      Selection Range:
        let f x = x + 1
            ^
    f -- test.ml @ 0
    ├─╴g -- test.ml @ 1
    │  ├─╴h -- test.ml @ 2
    │  │  ╰─╴foo -- test.ml @ 3
    │  ╰─╴bar -- test.ml @ 4
    ╰─╴baz -- test.ml @ 5
    |}];
  let%bind () = test ~line:2 ~character:4 in
  [%expect
    {|
    let h = g
        ^

      Range:
        let h = g
            ^
      Selection Range:
        let h = g
            ^
    h -- test.ml @ 2
    ╰─╴foo -- test.ml @ 3
    |}];
  let%bind () = test ~line:4 ~character:10 in
  [%expect
    {|
    let bar = g 7
              ^

      Range:
        let g = f
            ^
      Selection Range:
        let g = f
            ^
    g -- test.ml @ 1
    ├─╴h -- test.ml @ 2
    │  ╰─╴foo -- test.ml @ 3
    ╰─╴bar -- test.ml @ 4
    |}];
  return ()
;;

let%expect_test "circular aliases" =
  let source =
    {ocaml|
let h x = g x
let g x = h x
let h = g
;;
|ocaml}
  in
  let test = test_incoming source in
  let%bind () = test ~line:1 ~character:10 in
  [%expect
    {|
    let g x = h x
              ^

      Range:
        let h x = g x
            ^
      Selection Range:
        let h x = g x
            ^
    h -- test.ml @ 0
    ╰─╴g -- test.ml @ 1
       ╰─╴h -- test.ml @ 2
    |}];
  return ()
;;
