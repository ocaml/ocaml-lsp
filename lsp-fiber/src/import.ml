open Stdune
module List = Stdune.List
module Result = Stdune.Result
module Hashtbl = Stdune.Hashtbl
module Option = Stdune.Option
module Int = Stdune.Int
module Dyn = Stdune.Dyn
module Ordering = Stdune.Ordering
module Exn = Stdune.Exn
module Unix_env = Stdune.Env
module Fpath = Stdune.Path
module Code_error = Code_error
module Or_exn = Or_exn
module Table = Table
module Id = Id
module Exn_with_backtrace = Exn_with_backtrace
module Fdecl = Fdecl
module Queue = Queue
module Header = Lsp.Header
module Io = Lsp.Io
include Fiber_unix

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

  let field fields name conv = List.assoc_opt name fields |> Option.map ~f:conv

  let field_exn fields name conv =
    match field fields name conv with
    | Some f -> f
    | None -> error "Jsonrpc.Result.t: missing field" (`Assoc fields)

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

  module Conv = struct
    include Ppx_yojson_conv_lib.Yojson_conv
  end

  module O = struct
    let ( <|> ) c1 c2 json =
      match c1 json with
      | s -> s
      | exception Conv.Of_yojson_error (_, _) -> c2 json
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

  module Nullable_option = struct
    type 'a t = 'a option

    let t_of_yojson f = function
      | `Null -> None
      | json -> Some (f json)

    let yojson_of_t f = function
      | None -> assert false
      | Some s -> f s
  end

  module Void = struct
    type t

    let t_of_yojson = error "Void.t"

    let yojson_of_t (_ : t) = assert false
  end
end

module Fiber = struct
  include Fiber

  module Result = struct
    type nonrec ('a, 'e) t = ('a, 'e) result Fiber.t

    let lift x = Fiber.map x ~f:(fun x -> Ok x)

    let return x = Fiber.return (Ok x)

    let ( >>= ) x f =
      Fiber.bind
        ~f:(function
          | Error _ as e -> Fiber.return e
          | Ok x -> f x)
        x

    module O = struct
      let ( let+ ) x f = Fiber.map ~f:(Result.map ~f) x

      let ( let* ) x f = x >>= f
    end
  end
end

module Log = struct
  let level : (string option -> bool) ref = ref (fun _ -> false)

  let out = ref Format.err_formatter

  type message =
    { message : string
    ; payload : (string * Json.t) list
    }

  let msg message payload = { message; payload }

  let log ?section k =
    if !level section then (
      let message = k () in
      (match section with
      | None -> Format.fprintf !out "%s@." message.message
      | Some section -> Format.fprintf !out "[%s] %s@." section message.message);
      (match message.payload with
      | [] -> ()
      | fields -> Format.fprintf !out "%a@." Json.pp (`Assoc fields));
      Format.pp_print_flush !out ()
    )
end

let sprintf = Stdune.sprintf

module Types = Lsp.Types
module Client_request = Lsp.Client_request
module Server_request = Lsp.Server_request
module Server_notification = Lsp.Server_notification
module Client_notification = Lsp.Client_notification

module Jrpc_id = struct
  include Jsonrpc.Id

  let to_dyn = function
    | `String s -> Dyn.String s
    | `Int i -> Dyn.Int i
end
