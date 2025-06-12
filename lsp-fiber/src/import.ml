module List = Stdlib.ListLabels
module Code_error = Stdune.Code_error
module Fdecl = Stdune.Fdecl
module Header = Lsp.Header
module Io = Lsp.Io

module Json = struct
  include Lsp.Import.Json

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
    | Record r -> `Assoc (List.map r ~f:(fun (k, v) -> k, of_dyn v))
    | Variant (name, args) -> `Assoc [ name, of_dyn (List args) ]
    | Set xs -> `List (List.map ~f:of_dyn xs)
    | Map map -> `List (List.map map ~f:(fun (k, v) -> `List [ of_dyn k; of_dyn v ]))
  ;;

  let rec to_dyn (t : t) : Dyn.t =
    match t with
    | `String s -> String s
    | `Int i -> Int i
    | `Float f -> Float f
    | `Bool f -> Bool f
    | `Assoc o -> Record (List.map o ~f:(fun (k, v) -> k, to_dyn v))
    | `List l -> List (List.map l ~f:to_dyn)
    | `Tuple args -> Tuple (List.map args ~f:to_dyn)
    | `Null -> Dyn.Variant ("Null", [])
    | `Variant (name, Some arg) -> Variant (name, [ to_dyn arg ])
    | `Variant (name, None) -> Variant (name, [])
    | `Intlit s -> String s
  ;;
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
    if !level section
    then (
      let message = k () in
      (match section with
       | None -> Format.fprintf !out "%s@." message.message
       | Some section -> Format.fprintf !out "[%s] %s@." section message.message);
      (match message.payload with
       | [] -> ()
       | fields -> Format.fprintf !out "%a@." Json.pp (`Assoc fields));
      Format.pp_print_flush !out ())
  ;;
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
  ;;
end
