module Barrier = Fiber_unix.Private.Barrier

let print_result x =
  print_endline
    ( match x with
    | Ok () -> "ok"
    | Error (`Closed (`Read b)) -> Printf.sprintf "closed: %b" b
    | Error `Timeout -> "timeout" )

let%expect_test "create & close" =
  let b = Barrier.create () in
  Barrier.close b;
  [%expect {||}]

let%expect_test "write" =
  let b = Barrier.create () in
  Barrier.signal b;
  Barrier.close b;
  [%expect {||}]

let%expect_test "timeout" =
  let b = Barrier.create () in
  print_result (Barrier.await b ~timeout:0.1);
  Barrier.close b;
  [%expect {|
    timeout |}]

let%expect_test "read and write" =
  let b = Barrier.create () in
  Barrier.signal b;
  Barrier.signal b;
  print_result (Barrier.await b ~timeout:0.1);
  print_result (Barrier.await b ~timeout:0.1);
  Barrier.close b;
  [%expect {|
    ok
    timeout |}]
