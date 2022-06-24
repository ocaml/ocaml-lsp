open Stdune
module Bytes = BytesLabels
open Lev_fiber_util
module B = Bip_buffer

let%expect_test "is_empty" =
  let len = 100 in
  assert (B.is_empty (B.create (Bytes.create len) ~len));
  [%expect {||}]

let%expect_test "read empty" =
  let b = B.create (Bytes.create 0) ~len:1 in
  assert (B.peek b = None);
  [%expect {||}]

let peek_str b ~len =
  match B.peek b with
  | None -> assert false
  | Some s ->
      assert (s.len >= len);
      printfn "Requested %d. Available %d" len s.len;
      let get len =
        let dst = Bytes.create len in
        let src = B.buffer b in
        Bytes.blit ~dst ~dst_pos:0 ~src ~src_pos:s.pos ~len;
        Bytes.to_string dst
      in
      let peek = get len in
      if len = s.len then printfn "Peek: %S" peek
      else printfn "Peek: %S (full: %S)" peek (get s.len);
      peek

let print b =
  let pp_slice fmt (b, { B.Slice.pos; len }) =
    Format.fprintf fmt "%S" (Bytes.sub_string b ~pos ~len)
  in
  Format.printf "%a@." (B.pp pp_slice) b

let write_str b src =
  let len = String.length src in
  match B.reserve b ~len with
  | None -> assert false
  | Some dst_pos ->
      let dst = B.buffer b in
      Bytes.blit_string ~dst ~dst_pos ~src ~src_pos:0 ~len;
      B.commit b ~len

let%expect_test "bip buffers" =
  let buf_size = 16 in
  let b = B.create (Bytes.create buf_size) ~len:buf_size in
  assert (B.is_empty b);
  [%expect {| |}];
  let () =
    let mystr = "Test Foo|Bar" in
    let mystr_len = String.length mystr in
    write_str b mystr;
    assert (B.length b = mystr_len)
  in
  [%expect {| |}];
  (* Now we try to read 4 characters *)
  let () =
    let read_len = 8 in
    print b;
    B.junk b ~len:read_len
  in
  [%expect {|
    "Test Foo|Bar" |}];
  print b;
  [%expect {|
    "|Bar" |}]

let%expect_test "fill buffer" =
  let str = "foo bar baz foo" in
  let len = String.length str in
  let b = B.create (Bytes.create len) ~len in
  write_str b str;
  let str' = peek_str b ~len in
  assert (String.equal str str');
  [%expect {|
    Requested 15. Available 15
    Peek: "foo bar baz foo" |}]

let%expect_test "reserve overflow" =
  let buf_size = 16 in
  let b = B.create (Bytes.create buf_size) ~len:buf_size in
  let len = 17 in
  (match B.reserve b ~len with None -> () | Some _ -> assert false);
  [%expect {||}]

let%expect_test "unused space" =
  let buf_size = 16 in
  let half = buf_size / 2 in
  let b = B.create (Bytes.create buf_size) ~len:buf_size in
  let unused = B.unused_space b in
  printfn "unused space: %d" unused;
  [%expect {|
    unused space: 16 |}];
  assert (unused = buf_size);
  write_str b (String.make half 'a');
  assert (B.unused_space b = half);
  write_str b (String.make (pred half) 'b');
  B.junk b ~len:half;
  let unused = B.unused_space b in
  printfn "unused space: %d" unused;
  assert (unused = 9);
  [%expect {|
    unused space: 9 |}];
  let b = B.create (Bytes.create buf_size) ~len:buf_size in
  write_str b (String.make half 'a');
  assert (B.length b = half);
  B.junk b ~len:1;
  assert (B.length b = pred half);
  let unused = B.unused_space b in
  printfn "unused space: %d" unused;
  assert (unused = 9);
  [%expect {| unused space: 9 |}]

let blit ~src ~src_pos ~dst ~dst_pos ~len =
  Bytes.blit ~src ~src_pos ~dst ~dst_pos ~len

let%expect_test "resize" =
  let buf_size = 16 in
  let b = B.create (Bytes.make buf_size '0') ~len:buf_size in
  write_str b "00000";
  print b;
  [%expect {|
    "00000" |}];
  B.resize b blit (Bytes.make (buf_size * 2) '1') ~len:(buf_size * 2);
  print b;
  [%expect {|
    "00000" |}]

let%expect_test "compression - a only" =
  let buf_size = 8 in
  let b = B.create (Bytes.make buf_size '0') ~len:buf_size in
  write_str b "00000";
  B.junk b ~len:2;
  printfn "available: %d" (B.max_available b);
  [%expect {| available: 3 |}];
  B.compress b blit;
  print b;
  [%expect {|
    "000" |}];
  printfn "available: %d" (B.max_available b);
  [%expect {|
    available: 5 |}]

let%expect_test "compression - a & b" =
  let buf_size = 8 in
  let b = B.create (Bytes.make buf_size '0') ~len:buf_size in
  write_str b (String.make 7 '1');
  B.junk b ~len:6;
  print b;
  [%expect {| "1" |}];
  write_str b (String.make 3 '2');
  print b;
  [%expect {| "1""222" |}];
  printfn "available: %d" (B.max_available b);
  [%expect {| available: 3 |}];
  B.compress b blit;
  printfn "available: %d" (B.max_available b);
  [%expect {| available: 4 |}];
  print b;
  [%expect {| "1222" |}]

let%expect_test "reserve and commit b" =
  let buf_size = 8 in
  let b = B.create (Bytes.make buf_size '0') ~len:buf_size in
  write_str b (String.make 4 '1');
  write_str b (String.make 2 '1');
  assert (B.reserve b ~len:4 = None);
  B.junk b ~len:4;
  print b;
  [%expect {| "11" |}];
  write_str b (String.make 3 '2');
  print b;
  [%expect {| "11""222" |}];
  write_str b (String.make 1 '3');
  print b;
  [%expect {| "11""2223" |}]
