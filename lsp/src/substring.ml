open Import

type t =
  { pos : int
  ; len : int
  ; base : string
  }

let of_slice base ~pos ~len =
  assert (pos >= 0 && pos + len <= String.length base);
  assert (len >= 0);
  { base; pos; len }

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

let drop t len =
  assert (len >= 0);
  let len = min len t.len in
  let pos = t.pos + len in
  let len = t.len - len in
  { t with pos; len }

let take t len =
  assert (len >= 0);
  let len = min t.len len in
  { t with len }

let to_string { base; len; pos } = String.sub base ~pos ~len

let add_buffer { base; len; pos } buf = Buffer.add_substring buf base pos len

let split_at t n = (take t n, drop t n)

let rsplit_at t n =
  let n = t.len - n in
  split_at t n

let index_from =
  let rec loop s pos len c =
    if pos >= len then None
    else if s.[pos] = c then Some pos
    else loop s (pos + 1) len c
  in
  fun t ~pos c ->
    match loop t.base (t.pos + pos) (t.pos + t.len) c with
    | None -> None
    | Some pos -> Some (pos - t.pos)

let rindex =
  let rec loop s pos outside c =
    if pos <= outside then None
    else if s.[pos] = c then Some pos
    else loop s (pos - 1) outside c
  in
  fun t c -> loop t.base (t.len + t.pos - 1) (t.pos - 1) c

type termination =
  { newline : bool
  ; consumed : int
  }

let count_upto_chars_or_newline_backwards =
  let rec loop base pos outside =
    (* Format.eprintf "base = %S pos = %d outside %d@.%!" base pos outside; *)
    if pos <= outside then { newline = false; consumed = pos }
    else if base.[pos] = '\n' then { newline = true; consumed = pos }
    else loop base (pos - 1) outside
  in
  fun t n ->
    let last = t.pos + t.len - 1 in
    let termination =
      let outside = max (t.pos - 1) (last - n) in
      loop t.base last outside
    in
    { termination with consumed = last - termination.consumed }

let count_upto_chars_or_newline =
  let rec loop base pos last =
    if pos >= last then { newline = false; consumed = pos }
    else if base.[pos] = '\n' then { newline = true; consumed = pos }
    else loop base (pos + 1) last
  in
  fun t n ->
    let termination = loop t.base t.pos (t.pos + min n t.len) in
    { termination with consumed = termination.consumed - t.pos }

let blit t ~dst ~dst_pos =
  Bytes.blit_string ~src:t.base ~src_pos:t.pos ~len:t.len ~dst ~dst_pos

module Map = MoreLabels.Map.Make (struct
  type nonrec t = t

  let compare = compare
end)
