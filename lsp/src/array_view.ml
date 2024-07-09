type 'a t =
  { arr : 'a array
  ; start : int
  ; end_excl : int
  }

let make ?len arr ~pos =
  let arr_len = Array.length arr in
  if pos < 0 || pos > Array.length arr
  then
    invalid_arg
      (Printf.sprintf
         "Array_view.make: expected pos to be in [0, %d] but received %d"
         arr_len
         pos);
  let length = Option.value len ~default:(arr_len - pos) in
  let view_last_idx = pos + length in
  if view_last_idx > arr_len
  then
    invalid_arg
      (Printf.sprintf
         "Array_view.make: view's last idx = %d occurs after the array length = %d"
         view_last_idx
         arr_len);
  { arr; start = pos; end_excl = pos + length }
;;

let offset_index t i =
  let ix = t.start + i in
  if ix < t.end_excl then ix else invalid_arg "subarray index out of bounds"
;;

let get t i = t.arr.(offset_index t i)
let set t i x = t.arr.(offset_index t i) <- x
let length t = t.end_excl - t.start
let is_empty t = length t = 0

let common_suffix_len ai aj =
  if length ai = 0 || length aj = 0
  then 0
  else (
    let i = ref (length ai - 1) in
    let j = ref (length aj - 1) in
    while !i >= 0 && !j >= 0 && get ai !i = get aj !j do
      decr i;
      decr j
    done;
    length ai - !i - 1)
;;

let fold_left =
  let rec loop arr acc f j i =
    if Int.equal i j then acc else loop arr (f acc arr.(i)) f j (i + 1)
  in
  fun t ~init ~f -> loop t.arr init f t.end_excl t.start
;;

let iteri t ~f =
  for i = 0 to t.end_excl - t.start - 1 do
    f i t.arr.(t.start + i)
  done
;;

let sub t ~pos ~len =
  assert (len <= length t);
  let pos = t.start + pos in
  make t.arr ~pos ~len
;;

let blit t arr ~pos =
  let len = t.end_excl - t.start in
  ArrayLabels.blit ~src:t.arr ~src_pos:t.start ~dst:arr ~dst_pos:pos ~len
;;

let copy t = Array.init (t.end_excl - t.start) (fun i -> t.arr.(t.start + i))
let backing_array_pos t p = t.start + p
