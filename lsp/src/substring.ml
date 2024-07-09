open Import

type t =
  { pos : int
  ; len : int
  ; base : string
  }

let empty = { pos = 0; len = 0; base = "" }

let of_slice base ~pos ~len =
  assert (pos >= 0 && pos + len <= String.length base);
  assert (len >= 0);
  { base; pos; len }
;;

let of_string base = { base; len = String.length base; pos = 0 }

exception Result of int

let length t = t.len

let compare t { pos; len; base } =
  try
    for i = 0 to min t.len len - 1 do
      match Char.compare t.base.[i + t.pos] base.[i + pos] with
      | 0 -> ()
      | i -> raise_notrace (Result i)
    done;
    t.len - len
  with
  | Result r -> r
;;

let concat arr =
  let len = Array_view.fold_left ~f:(fun acc a -> acc + a.len) ~init:0 arr in
  let dst = Bytes.make len '\000' in
  let dst_pos = ref 0 in
  for i = 0 to Array_view.length arr - 1 do
    let { pos = src_pos; len; base = src } = Array_view.get arr i in
    Bytes.blit_string ~src ~src_pos ~dst ~dst_pos:!dst_pos ~len;
    dst_pos := !dst_pos + len
  done;
  Bytes.unsafe_to_string dst
;;

let drop t len =
  if len = t.len
  then empty
  else if len = 0
  then t
  else (
    assert (len > 0);
    let len = min len t.len in
    let pos = t.pos + len in
    let len = t.len - len in
    { t with pos; len })
;;

let take t len =
  if len = t.len
  then t
  else if len = 0
  then empty
  else (
    assert (len > 0);
    let len = min t.len len in
    { t with len })
;;

let to_string { base; len; pos } = String.sub base ~pos ~len
let add_buffer { base; len; pos } buf = Buffer.add_substring buf base pos len
let split_at t n = take t n, drop t n

let rsplit_at t n =
  let n = t.len - n in
  split_at t n
;;

let index_from =
  let rec loop s pos len c =
    if pos >= len then None else if s.[pos] = c then Some pos else loop s (pos + 1) len c
  in
  fun t ~pos c ->
    if pos < 0 || pos >= t.len then invalid_arg "Substring.index_from: out of bounds";
    match loop t.base (t.pos + pos) (t.pos + t.len) c with
    | None -> None
    | Some pos -> Some (pos - t.pos)
;;

let rindex_from =
  let rec loop s pos outside c =
    if pos <= outside
    then None
    else if s.[pos] = c
    then Some pos
    else loop s (pos - 1) outside c
  in
  fun t ~pos c ->
    if pos < 0 || pos > t.len then invalid_arg "Substring.rindex_from: out of bounds";
    match loop t.base (t.pos + pos - 1) (t.pos - 1) c with
    | None -> None
    | Some c -> Some (c - t.pos)
;;

let get_exn t i =
  if i < t.len then t.base.[t.pos + i] else invalid_arg "Substring.get: out of bounds"
;;

let rindex =
  let rec loop s pos outside c =
    if pos <= outside
    then None
    else if s.[pos] = c
    then Some pos
    else loop s (pos - 1) outside c
  in
  fun t c -> loop t.base (t.len + t.pos - 1) (t.pos - 1) c
;;

let blit t ~dst ~dst_pos =
  Bytes.blit_string ~src:t.base ~src_pos:t.pos ~len:t.len ~dst ~dst_pos
;;

type move =
  { newlines : int
  ; consumed : int
  }

let move_right =
  let rec loop base ~newlines ~pos ~outside =
    if pos = outside
    then newlines, pos
    else if base.[pos] = '\n'
    then loop base ~newlines:(newlines + 1) ~pos:(pos + 1) ~outside
    else loop base ~newlines ~pos:(pos + 1) ~outside
  in
  fun t ~pos ~len ->
    if pos = t.len
    then { newlines = 0; consumed = 0 }
    else (
      assert (len >= 0);
      assert (pos >= 0 && pos <= t.len);
      let real_pos = t.pos + pos in
      let outside = real_pos + min (t.len - pos) len in
      let newlines, final_pos = loop t.base ~newlines:0 ~pos:real_pos ~outside in
      { newlines; consumed = final_pos - real_pos })
;;

let move_left =
  let rec loop base ~newlines ~pos ~outside =
    if pos = outside
    then newlines, pos
    else if base.[pos] = '\n'
    then loop base ~newlines:(newlines + 1) ~pos:(pos - 1) ~outside
    else loop base ~newlines ~pos:(pos - 1) ~outside
  in
  fun t ~pos ~len ->
    if pos = 0
    then { newlines = 0; consumed = 0 }
    else (
      assert (pos >= 0 && pos <= t.len);
      let real_pos = t.pos + pos - 1 in
      let outside = max (t.pos - 1) (real_pos - len) in
      let newlines, final_pos = loop t.base ~newlines:0 ~pos:real_pos ~outside in
      { newlines; consumed = real_pos - final_pos })
;;

module Map = MoreLabels.Map.Make (struct
    type nonrec t = t

    let compare = compare
  end)

module Uutf = struct
  let src t ~pos decoder =
    let len = t.len - pos in
    if len > 0
    then (
      let pos = t.pos + pos in
      Uutf.Manual.src decoder (Bytes.unsafe_of_string t.base) pos len)
  ;;
end
