type 'a t =
  { buf : 'a option array
  ; mutable next_write_idx : int
  }

let create ~size =
  let buf = Array.init size (fun _ -> None) in
  let next_write_idx = 0 in
  { buf; next_write_idx }

let push_back ({ buf; next_write_idx } as t) elt =
  buf.(next_write_idx) <- Some elt;
  t.next_write_idx <- (next_write_idx + 1) mod Array.length buf

let to_list { buf; next_write_idx } =
  let rec loop ~stop_at ix acc =
    if stop_at ix then
      acc
    else
      loop ~stop_at (ix - 1)
        (match buf.(ix) with
        | None -> acc
        | Some x -> x :: acc)
  in
  loop ~stop_at:(fun ix -> ix < 0) (next_write_idx - 1) []
  |> loop ~stop_at:(fun ix -> ix < next_write_idx) (Array.length buf - 1)
