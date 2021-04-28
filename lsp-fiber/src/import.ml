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

module Json = struct
  type t = Ppx_yojson_conv_lib.Yojson.Safe.t

  let pp ppf (t : t) = Yojson.Safe.pretty_print ppf t
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
