open! Import
module Table = Hashtbl.Make (Lsp.Uri)

type t = Document.t Table.t

let make () = Table.create 50

let put store doc = Table.replace store ~key:(Document.uri doc) ~data:doc

let get_opt store = Table.find store

let get store uri =
  match Table.find store uri with
  | Some doc -> Ok doc
  | None ->
    Lsp.Import.Result.errorf "no document found with uri: %a" Lsp.Uri.pp uri

let remove_document store uri =
  Table.remove store uri

let get_size store = Table.length store
