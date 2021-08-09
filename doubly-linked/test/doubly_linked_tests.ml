open Stdune
module Dl = Doubly_linked

let printf = Printf.printf

let print_all pop q f =
  while not (Dl.is_empty q) do
    let elem = Option.value_exn (pop q) in
    printf "elem: %s\n" (f elem)
  done

let add_all add q xs = List.iter xs ~f:(fun x -> ignore (add q x))

let%expect_test "a newly created list is empty" =
  let q = Dl.create () in
  printf "new list is empty: %b" (Dl.is_empty q);
  [%expect {| new list is empty: true |}]

let%expect_test "pushing adds an element" =
  let q = Dl.create () in
  let (_ : int Dl.node) = Dl.append q 0 in
  printf "empty: %b\n" (Dl.is_empty q);
  [%expect {| empty: false |}]

let%expect_test "remove works with a 1 element queue" =
  let q = Dl.create () in
  let node = Dl.append q 0 in
  printf "empty: %b\n" (Dl.is_empty q);
  (match Dl.detach node with
  | Ok () -> printf "empty: %b\n" (Dl.is_empty q)
  | Error `Already_detached -> printf "already removed");
  [%expect {|
    empty: false
    empty: true |}]

let%expect_test "remove works with a 1 element queue" =
  let q = Dl.create () in
  let (_ : int Dl.node) = Dl.prepend q 0 in
  printf "empty: %b\n" (Dl.is_empty q);
  print_all Dl.detach_tail q Int.to_string;
  printf "empty: %b\n" (Dl.is_empty q);
  [%expect {|
    empty: false
    elem: 0
    empty: true |}]

let%expect_test "append all, detach head all" =
  let q = Dl.create () in
  add_all Dl.append q [ 1; 2; 3 ];
  print_all Dl.detach_head q Int.to_string;
  [%expect {|
    elem: 1
    elem: 2
    elem: 3 |}]

let%expect_test "prepend all, detach tail all" =
  let q = Dl.create () in
  add_all Dl.prepend q [ 1; 2; 3 ];
  print_all Dl.detach_tail q Int.to_string;
  [%expect {|
    elem: 1
    elem: 2
    elem: 3 |}]

let%expect_test "remove in the middle" =
  let dl = Dl.create () in
  add_all Dl.append dl [ 0; 1 ];
  let elem = Dl.append dl 2 in
  add_all Dl.append dl [ 3 ];
  (match Dl.detach elem with
  | Ok () -> printf "removed\n"
  | Error `Already_detached -> printf "already removed\n");
  print_all Dl.detach_head dl Int.to_string;
  [%expect {|
    removed
    elem: 0
    elem: 1
    elem: 3 |}]

let%expect_test "push; push; remove; push -- head & tail are set correctly" =
  let dl = Dl.create () in
  ignore @@ Dl.append dl 0;
  let n = Dl.append dl 1 in
  (match Dl.detach n with
  | Ok () -> printf "removed\n"
  | Error `Already_detached -> printf "already removed\n");
  ignore @@ Dl.append dl 2;
  print_all Dl.detach_head dl Int.to_string;
  [%expect {|
    removed
    elem: 0
    elem: 2 |}]
