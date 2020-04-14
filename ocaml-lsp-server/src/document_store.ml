open! Import
module Table = Hashtbl.Make (Lsp.Uri)

type value =
  { document : Document.t
  ; last_update : float
  }

type t = value Table.t

let make () = Table.create 50

let put store doc =
  Table.replace store ~key:(Document.uri doc)
    ~data:{ document = doc; last_update = Unix.time () }

let get_opt store uri =
  Table.find store uri |> Option.map ~f:(fun v -> v.document)

let get_full_opt store uri =
  Table.find store uri |> Option.map ~f:(fun v -> (v.document, v.last_update))

let get store uri =
  match Table.find store uri with
  | Some doc -> Ok doc.document
  | None ->
    Error
      (Lsp.Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:
           (Format.asprintf "no document found with uri: %a" Lsp.Uri.pp uri)
         ())

let remove_document store uri = Table.remove store uri

let get_size store = Table.length store
