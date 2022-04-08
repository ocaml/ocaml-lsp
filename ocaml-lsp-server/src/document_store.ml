open! Import
open Fiber.O

type t = (Uri.t, Document.t) Table.t

let make () = Table.create (module Uri) 50

let put store doc = Table.set store (Document.uri doc) doc

let get_opt store = Table.find store

let get store uri =
  match Table.find store uri with
  | Some doc -> doc
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make ~code:InvalidRequest
         ~message:
           (Format.asprintf "no document found with uri: %s" (Uri.to_string uri))
         ())

let close_document t uri =
  Fiber.of_thunk (fun () ->
      match Table.find t uri with
      | None -> Fiber.return ()
      | Some doc ->
        let+ () = Document.close doc in
        Table.remove t uri)

let close_all t =
  Fiber.of_thunk (fun () ->
      let docs = Table.fold t ~init:[] ~f:(fun doc acc -> doc :: acc) in
      let+ () = Fiber.parallel_iter docs ~f:Document.close in
      Table.clear t)
