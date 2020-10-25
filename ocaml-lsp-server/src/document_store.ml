open! Import

type t = (Uri.t, Document.t) Table.t

let make () = Table.create (module Uri) 50

let put store doc = Table.set store (Document.uri doc) doc

let get_opt store = Table.find store

let get store uri =
  match Table.find store uri with
  | Some doc -> Ok doc
  | None ->
    Error
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:(Format.asprintf "no document found with uri: %a" Uri.pp uri)
         ())

let remove_document store uri =
  match Table.find store uri with
  | None -> ()
  | Some doc ->
    Document.close doc;
    Table.remove store uri

let get_size store = Table.length store

let close t =
  Table.iter t ~f:Document.close;
  Table.clear t
