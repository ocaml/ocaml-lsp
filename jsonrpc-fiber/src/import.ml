module List = ListLabels

include struct
  (* TODO remove stdune dependence *)

  open Stdune
  module Code_error = Code_error
  module Exn_with_backtrace = Exn_with_backtrace
end

include struct
  open Jsonrpc
  module Id = Id
  module Response = Response
  module Request = Request
  module Notification = Notification
  module Packet = Packet
end

module Json = struct
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

  let to_pretty_string (t : t) = Yojson.Safe.pretty_to_string ~std:false t

  let error = Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error

  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t

  let rec of_dyn (t : Dyn.t) : t =
    match t with
    | Opaque -> `String "<opaque>"
    | Unit -> `String "()"
    | Int i -> `Int i
    | Int32 i -> `Int (Int32.to_int i)
    | Nativeint i -> `Int (Nativeint.to_int i)
    | Int64 i -> `Int (Int64.to_int i)
    | Bool b -> `Bool b
    | String s -> `String s
    | Bytes s -> `String (Bytes.to_string s)
    | Char c -> `String (String.make 1 c)
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
      Format.pp_print_flush !out ())
end

let sprintf = Printf.sprintf

let () =
  Printexc.register_printer (function
      | Jsonrpc.Response.Error.E t ->
        let json = Jsonrpc.Response.Error.yojson_of_t t in
        Some ("jsonrpc response error " ^ Json.to_pretty_string (json :> Json.t))
      | _ -> None)
