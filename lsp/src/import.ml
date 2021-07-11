module List = Stdlib.ListLabels
module Option = Stdlib.Option

module Result = struct
  include Stdlib.Result

  module O = struct
    let ( let+ ) x f = Stdlib.Result.map f x
  end
end

(* TODO remove these last remnants of stdnue once there is something public
   available *)
module Dyn = Stdune.Dyn
module Code_error = Stdune.Code_error

let sprintf = Stdune.sprintf

module String = struct
  include Stdune.String

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

module Json = struct
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

  let to_pretty_string (t : t) = Yojson.Safe.pretty_to_string ~std:false t

  let to_string t = Yojson.Safe.to_string t

  let of_string s = Yojson.Safe.from_string s

  let yojson_of_t x = x

  let t_of_yojson x = x

  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

  let yojson_of_list = Ppx_yojson_conv_lib.Yojson_conv.yojson_of_list

  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t

  module Jsonable = Ppx_yojson_conv_lib.Yojsonable

  let bool b = `Bool b

  let field fields name conv = List.assoc_opt name fields |> Option.map conv

  let field_exn fields name conv =
    match field fields name conv with
    | Some f -> f
    | None -> error ("missing field" ^ name) (`Assoc fields)

  let rec of_dyn (t : Dyn.t) : t =
    match t with
    | Opaque -> `String "<opaque>"
    | Unit -> `String "()"
    | Int i -> `Int i
    | Int64 i -> `Int (Int64.to_int i)
    | Bool b -> `Bool b
    | String s -> `String s
    | Bytes s -> `String (Bytes.to_string s)
    | Char c -> `String (String.of_list [ c ])
    | Float f -> `Float f
    | Option None -> `String "<none>"
    | Option (Some s) -> of_dyn s
    | List xs -> `List (List.map ~f:of_dyn xs)
    | Array xs -> `List (List.map ~f:of_dyn (Array.to_list xs))
    | Tuple xs -> `List (List.map ~f:of_dyn xs)
    | Record r -> `Assoc (List.map r ~f:(fun (k, v) -> (k, of_dyn v)))
    | Variant (name, args) -> `Assoc [ (name, of_dyn (List args)) ]
    | Set xs -> `List (List.map ~f:of_dyn xs)
    | Map map ->
      `List (List.map map ~f:(fun (k, v) -> `List [ of_dyn k; of_dyn v ]))

  let rec to_dyn (t : t) : Dyn.t =
    match t with
    | `String s -> String s
    | `Int i -> Int i
    | `Float f -> Float f
    | `Bool f -> Bool f
    | `Assoc o -> Record (List.map o ~f:(fun (k, v) -> (k, to_dyn v)))
    | `List l -> List (List.map l ~f:to_dyn)
    | `Tuple args -> Tuple (List.map args ~f:to_dyn)
    | `Null -> Dyn.Variant ("Null", [])
    | `Variant (name, Some arg) -> Variant (name, [ to_dyn arg ])
    | `Variant (name, None) -> Variant (name, [])
    | `Intlit s -> String s

  module Conv = struct
    include Ppx_yojson_conv_lib.Yojson_conv
  end

  module O = struct
    let ( <|> ) c1 c2 json =
      match c1 json with
      | s -> s
      | (exception Jsonrpc.Json.Of_json (_, _))
      | (exception Conv.Of_yojson_error (_, _)) ->
        c2 json
  end

  module Option = struct
    type 'a t = 'a option

    let yojson_of_t f = function
      | None -> `Null
      | Some x -> f x

    let t_of_yojson f = function
      | `Null -> None
      | json -> Some (f json)
  end

  module Of = struct
    let list = Ppx_yojson_conv_lib.Yojson_conv.list_of_yojson

    let pair f g json =
      match json with
      | `List [ x; y ] -> (f x, g y)
      | json -> error "pair" json

    let int_pair =
      let int = Ppx_yojson_conv_lib.Yojson_conv.int_of_yojson in
      pair int int

    let untagged_union (type a) name (xs : (t -> a) list) (json : t) : a =
      match
        List.find_map xs ~f:(fun conv ->
            try Some (conv json) with
            | Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (_, _) -> None)
      with
      | None -> error name json
      | Some x -> x

    let literal_field (type a) (name : string) (k : string) (v : string)
        (f : t -> a) (json : t) : a =
      match json with
      | `Assoc xs -> (
        let ks, xs =
          List.partition_map xs ~f:(fun (k', v') ->
              if k = k' then
                if `String v = v' then
                  Left k
                else
                  error (sprintf "%s: incorrect key %s" name k) json
              else
                Right (k', v'))
        in
        match ks with
        | [] -> error (sprintf "%s: key %s not found" name k) json
        | [ _ ] -> f (`Assoc xs)
        | _ :: _ -> error (sprintf "%s: multiple keys %s" name k) json)
      | _ -> error (sprintf "%s: not a record (key: %s)" name k) json
  end

  module To = struct
    let list f xs = `List (List.map ~f xs)

    let literal_field (type a) (k : string) (v : string) (f : a -> t) (t : a) :
        t =
      match f t with
      | `Assoc xs -> `Assoc ((k, `String v) :: xs)
      | _ -> Code_error.raise "To.literal_field" []

    let int_pair (x, y) = `List [ `Int x; `Int y ]
  end

  module Nullable_option = struct
    type 'a t = 'a option

    let t_of_yojson f = function
      | `Null -> None
      | json -> Some (f json)

    let yojson_of_t f = function
      | None -> assert false
      | Some s -> f s
  end

  module Assoc = struct
    type ('a, 'b) t = ('a * 'b) list

    let yojson_of_t f g xs =
      let f k =
        match f k with
        | `String s -> s
        | json -> error "Json.Assoc.yojson_of_t not a string key" json
      in
      `Assoc (List.map xs ~f:(fun (k, v) -> (f k, g v)))

    let t_of_yojson f g json =
      let f s = f (`String s) in
      match json with
      | `Assoc xs -> List.map xs ~f:(fun (k, v) -> (f k, g v))
      | _ -> error "Json.Assoc.t_of_yojson: not an object" json
  end

  module Void = struct
    type t

    let t_of_yojson = error "Void.t"

    let yojson_of_t (_ : t) = assert false
  end

  let read_json_params f v =
    match f (Jsonrpc.Message.Structured.to_json v) with
    | r -> Ok r
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure msg, _)
      ->
      Error msg

  let require_params json =
    match json with
    | None -> Error "params are required"
    | Some params -> Ok params

  let message_params (t : _ Jsonrpc.Message.t) f =
    match require_params t.params with
    | Error e -> Error e
    | Ok x -> read_json_params f x
end

let sprintf = Stdune.sprintf
