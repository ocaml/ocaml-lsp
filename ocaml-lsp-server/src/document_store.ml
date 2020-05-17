open! Import

type t = (Lsp.Uri.t, Document.t) Table.t

let make () = Table.create (module Lsp.Uri) 50

let put store doc = Table.set store (Document.uri doc) doc

let get_opt store = Table.find store

let get store uri =
  match Table.find store uri with
  | Some doc -> Ok doc
  | None ->
    Error
      (Lsp.Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:
           (Format.asprintf "no document found with uri: %a" Lsp.Uri.pp uri)
         ())

let remove_document store uri = Table.remove store uri

let get_size store = Table.length store
