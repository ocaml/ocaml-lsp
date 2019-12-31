type json = Ppx_yojson_conv_lib.Yojson.Safe.t

let yojson_of_json x = x

let json_of_yojson x = x

module List = struct
  module List = ListLabels

  let[@ocaml.warning "-32"] filter_map xs ~f =
    List.fold_left xs ~init:[] ~f:(fun acc a ->
        match f a with
        | None -> acc
        | Some x -> x :: acc)
    |> List.rev

  let concat_map xs ~f = List.map xs ~f |> List.concat

  include List
end

module String = struct
  include StringLabels

  let for_all f t =
    let len = String.length t in
    let rec loop i = i = len || (f t.[i] && loop (i + 1)) in
    loop 0

  let reverse s1 =
    let len = length s1 in
    let s2 = Bytes.make len 'a' in
    for i = 0 to len - 1 do
      Bytes.set s2 i s1.[len - i - 1]
    done;
    Bytes.to_string s2

  let common_prefix_len s1 s2 =
    let rec aux i =
      if i >= length s1 || i >= length s2 || s1.[i] <> s2.[i] then
        i
      else
        aux (succ i)
    in
    aux 0

  (* [is_prefixed ~by s] returns [true] iff [by] is a prefix of [s] *)
  let is_prefixed ~by =
    let l = String.length by in
    fun s ->
      let l' = String.length s in
      l' >= l
      &&
      try
        for i = 0 to pred l do
          if s.[i] <> by.[i] then raise Not_found
        done;
        true
      with Not_found -> false

  (* Drop characters from beginning of string *)
  let drop n s = sub s ~pos:n ~len:(length s - n)

  module Set = struct
    include MoreLabels.Set.Make (struct
      type t = string

      let compare = compare
    end)

    let of_list l = List.fold_left ~f:(fun s elt -> add elt s) l ~init:empty

    let to_list s = fold ~f:(fun x xs -> x :: xs) s ~init:[]
  end

  module Map = struct
    include MoreLabels.Map.Make (struct
      type t = string

      let compare = compare
    end)

    let of_list l =
      List.fold_left ~f:(fun m (k, v) -> add ~key:k ~data:v m) l ~init:empty

    let to_list m = fold ~f:(fun ~key ~data xs -> (key, data) :: xs) m ~init:[]

    let keys m = fold ~f:(fun ~key ~data:_ xs -> key :: xs) m ~init:[]

    let values m = fold ~f:(fun ~key:_ ~data xs -> data :: xs) m ~init:[]

    let add_multiple key data t =
      let current = try find key t with Not_found -> [] in
      let data = data :: current in
      add ~key ~data t
  end

  let mem c s =
    try
      ignore (String.index s c : int);
      true
    with Not_found -> false

  let first_double_underscore_end s =
    let len = String.length s in
    let rec aux i =
      if i > len - 2 then
        raise Not_found
      else if s.[i] = '_' && s.[i + 1] = '_' then
        i + 1
      else
        aux (i + 1)
    in
    aux 0

  let no_double_underscore s =
    try
      ignore (first_double_underscore_end s);
      false
    with Not_found -> true

  let trim = function
    | "" -> ""
    | str ->
      let l = String.length str in
      let is_space = function
        | ' '
        | '\n'
        | '\t'
        | '\r' ->
          true
        | _ -> false
      in
      let r0 = ref 0
      and rl = ref l in
      while !r0 < l && is_space str.[!r0] do
        incr r0
      done;
      let r0 = !r0 in
      while !rl > r0 && is_space str.[!rl - 1] do
        decr rl
      done;
      let rl = !rl in
      if r0 = 0 && rl = l then
        str
      else
        sub str ~pos:r0 ~len:(rl - r0)

  let print () s = Printf.sprintf "%S" s

  (* FIXME: Remove once we drop support for 4.02 and replace the calls by their
     [_ascii] version. *)
  [@@@ocaml.warning "-3"]

  let capitalize = capitalize

  let uncapitalize = uncapitalize

  let lowercase = lowercase

  let uppercase = uppercase

  let split_on_char_ c s =
    match String.index s c with
    | exception Not_found -> [ s ]
    | p ->
      let rec loop i =
        match String.index_from s i c with
        | exception Not_found -> [ String.sub s i (String.length s - i) ]
        | j ->
          let s0 = String.sub s i (j - i) in
          s0 :: loop (j + 1)
      in
      let s0 = String.sub s 0 p in
      s0 :: loop (p + 1)

  let chop_prefix ~prefix text =
    let tlen = String.length text in
    let plen = String.length prefix in
    if tlen >= plen then
      try
        for i = 0 to plen - 1 do
          if prefix.[i] <> text.[i] then raise Not_found
        done;
        Some (String.sub text plen (tlen - plen))
      with Not_found -> None
    else
      None

  let next_occurrence ~pattern text from =
    let plen = String.length pattern in
    let last = String.length text - plen in
    let i = ref from
    and j = ref 0 in
    while !i <= last && !j < plen do
      if text.[!i + !j] <> pattern.[!j] then (
        incr i;
        j := 0
      ) else
        incr j
    done;
    if !j < plen then
      raise Not_found
    else
      !i

  let replace_all ~pattern ~with_ text =
    if pattern = "" then
      text
    else
      match next_occurrence ~pattern text 0 with
      | exception Not_found -> text
      | j0 ->
        let buffer = Buffer.create (String.length text) in
        let rec aux i j =
          Buffer.add_substring buffer text i (j - i);
          Buffer.add_string buffer with_;
          let i' = j + String.length pattern in
          match next_occurrence ~pattern text i' with
          | exception Not_found ->
            Buffer.add_substring buffer text i' (String.length text - i')
          | j' -> aux i' j'
        in
        aux 0 j0;
        Buffer.contents buffer
end

let let_ref r v f =
  let v' = !r in
  r := v;
  match f () with
  | result ->
    r := v';
    result
  | exception exn ->
    r := v';
    raise exn

external reraise : exn -> 'a = "%reraise"

module Result = struct
  let bind x ~f =
    match x with
    | Ok v -> f v
    | Error err -> Error err

  let map x ~f =
    match x with
    | Ok v -> Ok (f v)
    | Error err -> Error err

  let errorf fmt =
    let kerr _ = Error (Format.flush_str_formatter ()) in
    Format.kfprintf kerr Format.str_formatter fmt

  module Infix = struct
    let ( >>= ) x f = bind x ~f

    let ( >>| ) x f = map x ~f

    let return x = Ok x

    let errorf = errorf
  end
end

module Option = struct
  let map t ~f =
    match t with
    | None -> None
    | Some x -> Some (f x)
end

module Yojsonable = Ppx_yojson_conv_lib.Yojsonable
