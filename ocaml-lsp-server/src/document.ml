open! Import

module Kind = struct
  type t =
    | Intf
    | Impl

  let of_fname p =
    match Filename.extension p with
    | ".ml"
    | ".re" ->
      Impl
    | ".mli"
    | ".rei" ->
      Intf
    | ext -> failwith ("Unknown extension " ^ ext)
end

module Syntax = struct
  type t =
    | Ocaml
    | Reason
    | Ocamllex
    | Menhir

  let human_name = function
    | Ocaml -> "OCaml"
    | Reason -> "Reason"
    | Ocamllex -> "OCamllex"
    | Menhir -> "Menhir/ocamlyacc"

  let all =
    [ ("ocaml.interface", Ocaml)
    ; ("ocaml", Ocaml)
    ; ("reason", Reason)
    ; ("ocaml.ocamllex", Ocamllex)
    ; ("ocaml.menhir", Menhir)
    ]

  let of_fname s =
    match Filename.extension s with
    | ".mli"
    | ".ml" ->
      Ocaml
    | ".rei"
    | ".re" ->
      Reason
    | ".mll" -> Ocamllex
    | ".mly" -> Menhir
    | ext -> failwith ("Unknown extension " ^ ext)

  let of_language_id language_id =
    match List.assoc all language_id with
    | Some id -> id
    | None ->
      Code_error.raise "invalid language id"
        [ ("language_id", String language_id) ]

  let to_language_id x =
    List.find_map all ~f:(fun (k, v) -> Option.some_if (v = x) k)
    |> Option.value_exn

  let markdown_name = function
    | Ocaml -> "ocaml"
    | Reason -> "reason"
    | s -> to_language_id s
end

type t =
  { tdoc : Text_document.t
  ; source : Msource.t
  ; pipeline : Mpipeline.t
  ; config : Mconfig.t
  ; merlin : Scheduler.thread
  ; timer : Scheduler.timer
  }

let uri doc = Text_document.documentUri doc.tdoc

let kind t = Kind.of_fname (Uri.to_path (uri t))

let syntax t = Syntax.of_language_id (Text_document.languageId t.tdoc)

let timer t = t.timer

let source doc = doc.source

let with_pipeline (doc : t) f =
  Scheduler.async_exn doc.merlin (fun () ->
      Mpipeline.with_pipeline doc.pipeline (fun () -> f doc.pipeline))
  |> Scheduler.await_no_cancel

let with_pipeline_exn doc f =
  let open Fiber.O in
  let+ res = with_pipeline doc f in
  Result.ok_exn res

let version doc = Text_document.version doc.tdoc

let make_config uri =
  let path = Uri.to_path uri in
  let mconfig = Mconfig.initial in
  let path = Misc.canonicalize_filename path in
  let filename = Filename.basename path in
  let directory = Filename.dirname path in
  let mconfig =
    { mconfig with
      query = { mconfig.query with verbosity = 1; filename; directory }
    }
  in
  Mconfig.load_dotmerlins mconfig
    ~filenames:
      [ (let base = "." ^ filename ^ ".merlin" in
         Filename.concat directory base)
      ]

let make timer merlin_thread tdoc =
  let tdoc = Text_document.make tdoc in
  (* we can do that b/c all text positions in LSP are line/col *)
  let text = Text_document.text tdoc in
  let config = make_config (Text_document.documentUri tdoc) in
  let source = Msource.make text in
  let pipeline = Mpipeline.make config source in
  { tdoc; source; config; pipeline; merlin = merlin_thread; timer }

let update_text ?version change doc =
  let tdoc = Text_document.apply_content_change ?version change doc.tdoc in
  let text = Text_document.text tdoc in
  let config = make_config (Text_document.documentUri tdoc) in
  let source = Msource.make text in
  let pipeline = Mpipeline.make config source in
  { doc with tdoc; config; source; pipeline }

let dispatch (doc : t) command =
  with_pipeline doc (fun pipeline -> Query_commands.dispatch pipeline command)

let dispatch_exn (doc : t) command =
  with_pipeline_exn doc (fun pipeline ->
      Query_commands.dispatch pipeline command)

let close t = Scheduler.cancel_timer t.timer
