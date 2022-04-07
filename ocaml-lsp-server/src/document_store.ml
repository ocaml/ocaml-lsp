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

let remove_document store uri =
  Fiber.of_thunk (fun () ->
      match Table.find store uri with
      | None -> Fiber.return ()
      | Some doc ->
        let+ () = Document.close doc in
        Table.remove store uri)

let unregister_promotions t uris =
  let* () = Fiber.return () in
  List.filter uris ~f:(fun uri ->
      match Table.find t.db uri with
      | None -> false
      | Some doc ->
        let doc = { doc with promotions = doc.promotions - 1 } in
        let unsubscribe = doc.promotions = 0 in
        if unsubscribe && doc.document = None then
          Table.remove t.db uri
        else
          Table.set t.db uri doc;
        unsubscribe)
  |> unregister_request t

let register_promotions t uris =
  let* () = Fiber.return () in
  List.filter uris ~f:(fun uri ->
      let doc, subscribe =
        match Table.find t.db uri with
        | None -> ({ document = None; promotions = 0 }, true)
        | Some doc -> ({ doc with promotions = doc.promotions + 1 }, false)
      in
      Table.set t.db uri doc;
      subscribe)
  |> register_request t

let close t =
  Fiber.of_thunk (fun () ->
      let docs = Table.fold t ~init:[] ~f:(fun doc acc -> doc :: acc) in
      let+ () = Fiber.parallel_iter docs ~f:Document.close in
      Table.clear t)
