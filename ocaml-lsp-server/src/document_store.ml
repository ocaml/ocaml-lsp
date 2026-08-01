open! Import
open Fiber.O

type server = Server : 'a Server.t Fdecl.t -> server

type semantic_tokens_cache =
  { resultId : string
  ; tokens : int array
  }

(** Promote code actions may target files ocamllsp does not own (any path Dune
    can promote). Editors only request actions for associated documents, so
    closed promotion targets need a per-URI dynamic registration (DR).

    Open documents already go through the static codeAction provider, so they
    must not keep a DR.

    "X" means DR is required; "O" means it must be absent:

    {v
                          | Open | Closed |
                          -----------------
      Promotions Pending  |  O   |   X    |
      No Promotions       |  O   |   O    |
    v}

    Register on transitions into X; unregister on transitions out of X. *)

type doc =
  { document : Document.t option
  ; (* promotion refcount. dynamic registration is needed only while the
       document is closed and this count is positive *)
    promotions : int
  ; mutable semantic_tokens_cache : semantic_tokens_cache option
  }

type t =
  { db : (Uri.t, doc ref) Hashtbl.t
  ; server : server
  ; (* The pool is needed to run subscribe/unsubscribe requests. To prevent
       deadlocks with synchronous responses to lsp. In the future, these
       deadlocks should cause runtime errors or will just be impossible *)
    pool : Fiber.Pool.t
  }

let make s pool = { db = Hashtbl.create (module Uri); server = Server s; pool }
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
;;

let register_request t uris =
  match uris with
  | [] -> Fiber.return ()
  | _ :: _ ->
    let registrations =
      List.map uris ~f:(fun uri ->
        let id = code_action_id uri in
        let registerOptions =
          let documentSelector =
            [ `TextDocumentFilter
                (TextDocumentFilter.create ~pattern:(Uri.to_path uri) ())
            ]
          in
          CodeActionRegistrationOptions.create
            ~documentSelector
            ~codeActionKinds:[ CodeActionKind.QuickFix ]
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
;;

let open_document t doc =
  let* () = Fiber.return () in
  let key = Document.uri doc in
  match Hashtbl.find t.db key with
  | None ->
    Hashtbl.set
      t.db
      ~key
      ~data:(ref { document = Some doc; promotions = 0; semantic_tokens_cache = None });
    Fiber.return ()
  | Some d ->
    (* if there's no document, then we just opened it to track promotions.

       if there's a document already, we're doing a double open and there's no
       need to unregister. *)
    let unregister = !d.document = None in
    d := { !d with document = Some doc };
    if unregister then unregister_request t [ key ] else Fiber.return ()
;;

let get_opt t uri = Hashtbl.find t.db uri |> Option.bind ~f:(fun d -> !d.document)

let no_document_found uri = function
  | Some s -> s
  | None ->
    Jsonrpc.Response.Error.raise
      (Jsonrpc.Response.Error.make
         ~code:InvalidRequest
         ~message:(Format.asprintf "no document found with uri: %s" (Uri.to_string uri))
         ())
;;

let get' t uri = Hashtbl.find t.db uri |> no_document_found uri
let get t uri = !(get' t uri).document |> no_document_found uri

let change_document t uri ~f =
  let doc = get' t uri in
  let document = f (no_document_found uri !doc.document) in
  doc := { !doc with document = Some document };
  document
;;

let maybe_close_doc (doc : doc) =
  match doc.document with
  | None -> Fiber.return ()
  | Some d -> Document.close d
;;

let close_document t uri =
  Fiber.of_thunk (fun () ->
    match Hashtbl.find t.db uri with
    | None -> Fiber.return ()
    | Some doc ->
      let close_doc () = maybe_close_doc !doc in
      if !doc.promotions = 0
      then (
        Hashtbl.remove t.db uri;
        close_doc ())
      else (
        doc := { !doc with document = None };
        Fiber.fork_and_join_unit close_doc (fun () -> register_request t [ uri ])))
;;

let unregister_promotions t uris =
  let* () = Fiber.return () in
  List.filter uris ~f:(fun uri ->
    match Hashtbl.find t.db uri with
    | None -> false
    | Some doc ->
      doc := { !doc with promotions = !doc.promotions - 1 };
      let unsubscribe = !doc.promotions = 0 && !doc.document = None in
      if unsubscribe then Hashtbl.remove t.db uri;
      unsubscribe)
  |> unregister_request t
;;

let register_promotions t uris =
  let* () = Fiber.return () in
  List.filter uris ~f:(fun uri ->
    match Hashtbl.find t.db uri with
    | None ->
      let doc = ref { document = None; promotions = 1; semantic_tokens_cache = None } in
      Hashtbl.set t.db ~key:uri ~data:doc;
      true
    | Some doc ->
      doc := { !doc with promotions = !doc.promotions + 1 };
      false)
  |> register_request t
;;

let update_semantic_tokens_cache
  : t -> Uri.t -> resultId:string -> tokens:int array -> unit
  =
  fun t uri ~resultId ~tokens ->
  let doc = get' t uri in
  !doc.semantic_tokens_cache <- Some { resultId; tokens }
;;

let get_semantic_tokens_cache : t -> Uri.t -> semantic_tokens_cache option =
  fun t uri ->
  let doc = get' t uri in
  !doc.semantic_tokens_cache
;;

let parallel_iter t ~f =
  let all = Hashtbl.fold ~init:[] t.db ~f:(fun ~key:_ ~data:doc acc -> doc :: acc) in
  Fiber.parallel_iter all ~f:(fun doc ->
    match !doc.document with
    | None -> Fiber.return ()
    | Some document -> f document)
;;

let fold t ~init ~f =
  Hashtbl.fold t.db ~init ~f:(fun ~key:_ ~data:doc acc ->
    match !doc.document with
    | None -> acc
    | Some x -> f x acc)
;;

let close_all t =
  Fiber.of_thunk (fun () ->
    let docs = Hashtbl.fold t.db ~init:[] ~f:(fun ~key:_ ~data:doc acc -> !doc :: acc) in
    Hashtbl.clear t.db;
    Fiber.parallel_iter docs ~f:maybe_close_doc)
;;
