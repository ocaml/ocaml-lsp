module List = Stdune.List
module Hashtbl = Stdune.Hashtbl
module Option = Stdune.Option
module Either = Stdune.Either
module Int = Stdune.Int
module Dyn = Stdune.Dyn
module Ordering = Stdune.Ordering
module Exn = Stdune.Exn

module String = struct
  include Stdune.String

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

module Result = struct
  include Stdune.Result

  let errorf fmt =
    let kerr _ = Error (Format.flush_str_formatter ()) in
    Format.kfprintf kerr Format.str_formatter fmt
end

module Json = struct
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

  let yojson_of_t x = x

  let t_of_yojson x = x

  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

  module Jsonable = Ppx_yojson_conv_lib.Yojsonable

  let field fields name conv = List.assoc_opt name fields |> Option.map ~f:conv

  let field_exn fields name conv =
    match field fields name conv with
    | None -> error "Jsonrpc.Result.t: missing field" (`Assoc fields)
    | Some f -> f
end
