module Array = ArrayLabels
module Bytes = BytesLabels

type t =
  { pos : int
  ; len : int
  ; base : string
  }

let of_slice base ~pos ~len =
  assert (pos >= 0 && pos + len <= String.length base);
  assert (len >= 0);
  { base; pos; len }

exception Result of int

let compare t { pos; len; base } =
  try
    for i = 0 to min t.len len - 1 do
      match Char.compare t.base.[i + t.pos] base.[i + pos] with
      | 0 -> ()
      | i -> raise_notrace (Result i)
    done;
    t.len - len
  with Result r -> r

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

module Map = MoreLabels.Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
