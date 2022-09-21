open Test.Import
open Code_actions

let find_mapi ~f l =
  let rec k i = function
    | [] -> None
    | x :: xs -> (
      match f i x with
      | Some x' -> Some x'
      | None -> k (i + 1) xs)
  in
  k 0 l

let parse_cursor src =
  let cursor =
    String.split_lines src
    |> find_mapi ~f:(fun lnum line ->
           match String.index line '$' with
           | Some cnum -> Some (Position.create ~character:cnum ~line:lnum)
           | None -> None)
    |> Option.value_exn
  in
  ( String.filter_map src ~f:(function
        | '$' -> None
        | c -> Some c)
  , Range.create ~start:cursor ~end_:cursor )

let rec take n l =
  if n = 0 then []
  else
    match l with
    | [] -> failwith "list shorter than n"
    | x :: xs -> x :: take (n - 1) xs

let offset_of_position src (pos : Position.t) =
  let line_offset =
    String.split_lines src |> take pos.line
    |> List.fold_left ~init:0 ~f:(fun s l -> s + String.length l)
  in
  line_offset + pos.line (* account for line endings *) + pos.character

let apply_edit (edit : TextEdit.t) src =
  let start_offset = offset_of_position src edit.range.start in
  let end_offset = offset_of_position src edit.range.end_ in
  let start = String.take src start_offset in
  let end_ = String.drop src end_offset in
  start ^ edit.newText ^ end_

let apply_inline_action src range =
  let open Option.O in
  let code_actions = ref None in
  iter_code_actions src range (fun ca -> code_actions := Some ca);
  let* m_code_actions = !code_actions in
  let* code_actions = m_code_actions in
  let* { documentChanges; _ } =
    List.find_map code_actions ~f:(function
        | `CodeAction { kind = Some RefactorInline; edit = Some edit; _ } ->
          Some edit
        | _ -> None)
  in
  let+ documentChanges in
  let edits =
    List.filter_map documentChanges ~f:(function
        | `TextDocumentEdit e -> Some e
        | _ -> None)
  in
  match edits with
  | [] -> src
  | [ { edits = [ `TextEdit e ]; _ } ] -> apply_edit e src
  | _ -> failwith "expected one edit"

let inline_test src =
  let src, range = parse_cursor src in
  Option.iter (apply_inline_action src range) ~f:print_string

let%expect_test "" =
  inline_test {|
let _ =
  let $x = 0 in
  x + 1
|};
  [%expect {|
    let _ =
      let x = 0 in
      (0) + 1 |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x = x in
  f 1
|};
  [%expect {|
    let _ =
      let f x = x in
      (1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f _ = 0 in
  f 1
|};
  [%expect {|
    let _ =
      let f _ = 0 in
      (0) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x = x + x in
  f 1
|};
  [%expect {|
    let _ =
      let f x = x + x in
      (1 + 1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x = x + x in
  f (g 1)
|};
  [%expect
    {|
    let _ =
      let f x = x + x in
      (let x = g 1 in x + x) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x y = x + y in
  f 0
|};
  [%expect {|
    let _ =
      let f x y = x + y in
      (fun y -> 0 + y) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x ~y = x + y in
  f ~y:0
|};
  [%expect
    {|
    let _ =
      let f x ~y = x + y in
      ((fun x ~y -> x + y) ~y:0) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f ~x y = x + y in
  f ~x:0
|};
  [%expect {|
    let _ =
      let f ~x y = x + y in
      (fun y -> 0 + y) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f ~x ~y = x + y in
  f ~y:0
|};
  [%expect
    {|
    let _ =
      let f ~x ~y = x + y in
      (fun ~x -> x + 0) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f (x : int) = x + 1 in
  f 0
|};
  [%expect
    {|
    let _ =
      let f (x : int) = x + 1 in
      (0 + 1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f (type a) (x : a) = x in
  f 0
|};
  [%expect
    {|
    let _ =
      let f (type a) (x : a) = x in
      ((fun (type a) -> fun (x : a) -> x) 0) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f : int -> int = fun x -> x in
  f 0
|};
  [%expect
    {|
    let _ =
      let f : int -> int = fun x -> x in
      (0) |}]

let%expect_test "" =
  inline_test
    {|
let _ =
  let $f = function Some x -> x | None -> 0 in
  f (Some 1)
|};
  [%expect
    {|
    let _ =
      let f = function Some x -> x | None -> 0 in
      ((function | Some x -> x | None -> 0) (Some 1)) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f (x as y) = y + 1 in
  f 1
|};
  [%expect
    {|
    let _ =
      let f (x as y) = y + 1 in
      (let x as y = 1 in y + 1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f 1 = 2 in
  f 2
|};
  [%expect
    {|
    let _ =
      let f 1 = 2 in
      (let 1 = 2 in 2) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f (x, y) = x + y in
  f (1, 2)
|};
  [%expect
    {|
    let _ =
      let f (x, y) = x + y in
      (let (x, y) = (1, 2) in x + y) |}]

let%expect_test "" =
  inline_test
    {|
type t = { x : int; y : int }
let _ =
  let $f { x; y } = x + y in
  f { x = 1; y = 1 }
|};
  [%expect
    {|
    type t = { x : int; y : int }
    let _ =
      let f { x; y } = x + y in
      (let { x; y } = { x = 1; y = 1 } in x + y) |}]

let%expect_test "" =
  inline_test
    {|
type t = { x : int; y : int }
let _ =
  let $f { x; _ } = x + 1 in
  f { x = 1; y = 1 }
|};
  [%expect
    {|
    type t = { x : int; y : int }
    let _ =
      let f { x; _ } = x + 1 in
      (let { x;_} = { x = 1; y = 1 } in x + 1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x = [%test] x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = [%test] x in
      (([%test ]) 1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x = x in
  [%test] (f 1)
|};
  [%expect
    {|
    let _ =
      let f x = x in
      [%test] (1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x = (* test comment *) x in
  f 1
|};
  [%expect
    {|
    let _ =
      let f x = (* test comment *) x in
      (1) |}]

let%expect_test "" =
  inline_test {|
let _ =
  let $f x = x in
  (* test comment *) f 1
|};
  [%expect
    {|
    let _ =
      let f x = x in
      (* test comment *) (1) |}]
