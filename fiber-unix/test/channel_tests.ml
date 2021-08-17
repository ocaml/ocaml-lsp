module Channel = Fiber_unix.Private.Channel

let%expect_test "send/recv 0, 1 in concurrent threads; then close the channel" =
  let ch = Channel.create () in
  let th0 =
    Thread.create
      (fun () ->
        for _ = 0 to 1 do
          match Channel.get ch with
          | Ok i -> Printf.printf "%d\n" i
          | Error `Closed -> assert false
        done;
        match Channel.get ch with
        | Ok _ -> assert false
        | Error `Closed -> print_endline "closed")
      ()
  in
  let send_or_fail v =
    match Channel.send ch v with
    | Ok () -> ()
    | Error `Closed -> assert false
  in
  let th1 =
    Thread.create
      (fun () ->
        send_or_fail 0;
        send_or_fail 1;
        Channel.close ch)
      ()
  in
  Thread.join th0;
  Thread.join th1;
  [%expect {|
    0
    1
    closed |}]

let%expect_test "send 0, 1, 2, but remove 1" =
  let ch = Channel.create () in
  let print_int i = Printf.printf "%d\n" i in
  let th0 =
    Thread.create
      (fun () ->
        let v0 = Channel.get ch in
        print_int @@ Result.get_ok v0;
        Unix.sleepf 0.01;
        let v1 = Channel.get ch in
        print_int @@ Result.get_ok v1)
      ()
  in
  let th1 =
    Thread.create
      (fun () ->
        ignore @@ Channel.send ch 0;
        let removable = Channel.send_removable ch 1 in
        Channel.remove_if_not_consumed @@ Result.get_ok removable;
        Channel.send ch 2)
      ()
  in
  Thread.join th0;
  Thread.join th1;
  [%expect {|
    0
    2 |}]
