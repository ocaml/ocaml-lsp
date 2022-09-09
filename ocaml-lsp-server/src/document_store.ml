open! Import
open Fiber.O

type server = Server : 'a Server.t Fdecl.t -> server

type semantic_tokens_cache =
  { resultId : string
  ; tokens : int array
  }

(** The following code attempts to resolve the issue of displaying code actions
    for unopened document.

    Unopened documents require a dynamic registration (DR) for code actions,
    while open documents do not.

    Here are the four states of the documents and the DR status they require.
    "X" marks that DR is required while "O" marks that no Dr should be present

    {v
                          | Open | Closed |
                          -----------------
      Promotions Pending  |  O   |   X    |
      No Promotions       |  O   |   O    |
    v}

    From the above, we see that we need to unregister when transitioning from X
    to O and to register while transitioning from X to O. *)

type doc =
  { (* invariant: if [document <> None], then no promotions are active *)
    document : Document.t option
  ; (* the number of associated promotions. when this is 0, we may unsubscribe
       from code actions *)
    promotions : int
  ; mutable semantic_tokens_cache : semantic_tokens_cache option
  }

type t =
  { db : (Uri.t, doc) Table.t
  ; server : server
  ; (* The pool is needed to run subscribe/unsubscribe requests. To prevent
       deadlocks with synchronous responses to lsp. In the future, these
       deadlocks should cause runtime errors or will just be impossible *)
    pool : Fiber.Pool.t
  }

let make s pool = { db = Table.create (module Uri) 50; server = Server s; pool }

let code_action_id uri = "ocamllsp-promote/" ^ Uri.to_string uri

let method_ = "textDocument/codeAction"

let unregister_request t uris =
  match uris with
  | [] -> Fiber.return ()
  | _ :: _ ->
    let unregisterations =
      List.map uris ~f:(fun uri ->
          let id = code_action_id uri in
          Unregistration.create ~id ~method_)
    in
    let (Server server) = t.server in
    let server = Fdecl.get server in
    let req = UnregistrationParams.create ~unregisterations in
    Fiber.Pool.task t.pool ~f:(fun () ->
        Server.request server (Server_request.ClientUnregisterCapability req))

let register_request t uris =
  match uris with
  | [] -> Fiber.return ()
  | _ :: _ ->
    let registrations =
      List.map uris ~f:(fun uri ->
          let id = code_action_id uri in
          let registerOptions =
            let documentSelector =
              [ `DocumentFilter
                  (`TextDocumentFilter
                    (TextDocumentFilter.create ~pattern:(Uri.to_path uri) ()))
              ]
            in
            CodeActionRegistrationOptions.create
              ~documentSelector
              ~codeActionKinds:[ CodeActionKind.Other "Promote" ]
              ()
            |> CodeActionRegistrationOptions.yojson_of_t
          in
          Registration.create ~id ~method_ ~registerOptions ())
    in
    let (Server server) = t.server in
    let server = Fdecl.get server in
    let req = RegistrationParams.create ~registrations in
    Fiber.Pool.task t.pool ~f:(fun () ->
        Server.request server (Server_request.ClientRegisterCapability req))

let open_document t doc =
  let* () = Fiber.return () in
  let key = Document.uri doc in
  match Table.find t.db key with
  | None ->
    Table.set
      t.db
      key
      { document = Some doc; promotions = 0; semantic_tokens_cache = None };
    Fiber.return ()
  | Some d ->
    assert (d.document = None);
    Table.set t.db key { d with document = Some doc };
    unregister_request t [ key ]

let get_opt t uri = Table.find t.db uri |> Option.bind ~f:(fun d -> d.document)

let no_document_found uri = function
  | Some s -> s
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidRequest
         ~message:
           (Format.asprintf
              "no document found with uri: %s"
              (Uri.to_string uri))
         ())

let get' t uri = Table.find t.db uri |> no_document_found uri

let get t uri = (get' t uri).document |> no_document_found uri

let change_document t uri ~f =
  let doc = get' t uri in
  let document = f (no_document_found uri doc.document) in
  let doc = { doc with document = Some document } in
  Table.set t.db uri doc;
  document

let maybe_close_doc (doc : doc) =
  match doc.document with
  | None -> Fiber.return ()
  | Some d -> Document.close d

let close_document t uri =
  Fiber.of_thunk (fun () ->
      match Table.find t.db uri with
      | None -> Fiber.return ()
      | Some doc ->
        let* () = maybe_close_doc doc in
        if doc.promotions = 0 then (
          Table.remove t.db uri;
          Fiber.return ())
        else (
          Table.set t.db uri { doc with document = None };
          register_request t [ uri ]))

let unregister_promotions t uris =
  let* () = Fiber.return () in
  List.filter uris ~f:(fun uri ->
      match Table.find t.db uri with
      | None -> false
      | Some doc ->
        let doc = { doc with promotions = doc.promotions - 1 } in
        let unsubscribe = doc.promotions = 0 in
        if unsubscribe && doc.document = None then Table.remove t.db uri
        else Table.set t.db uri doc;
        unsubscribe)
  |> unregister_request t

let register_promotions t uris =
  let* () = Fiber.return () in
  List.filter uris ~f:(fun uri ->
      let doc, subscribe =
        match Table.find t.db uri with
        | None ->
          ( { document = None; promotions = 0; semantic_tokens_cache = None }
          , true )
        | Some doc -> ({ doc with promotions = doc.promotions + 1 }, false)
      in
      Table.set t.db uri doc;
      subscribe)
  |> register_request t

let update_semantic_tokens_cache :
    t -> Uri.t -> resultId:string -> tokens:int array -> unit =
 fun t uri ~resultId ~tokens ->
  let doc = get' t uri in
  doc.semantic_tokens_cache <- Some { resultId; tokens }

let get_semantic_tokens_cache : t -> Uri.t -> semantic_tokens_cache option =
 fun t uri ->
  let doc = get' t uri in
  doc.semantic_tokens_cache

let close_all t =
  Fiber.of_thunk (fun () ->
      let docs = Table.fold t.db ~init:[] ~f:(fun doc acc -> doc :: acc) in
      Table.clear t.db;
      Fiber.parallel_iter docs ~f:maybe_close_doc)
