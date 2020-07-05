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

  let of_language_id = function
    | "ocaml.interface" -> Ocaml
    | "ocaml" -> Ocaml
    | "reason" -> Reason
    | id -> failwith ("Unexpected language id " ^ id)

  let to_language_id = function
    | Ocaml -> "ocaml"
    | Reason -> "reason"
end

type t =
  { tdoc : Lsp.Text_document.t
  ; source : Msource.t
  ; pipeline : Mpipeline.t
  ; config : Mconfig.t
  ; merlin : Scheduler.thread
  ; timer : Scheduler.timer
  }

let uri doc = Lsp.Text_document.documentUri doc.tdoc

let kind t = Kind.of_fname (Lsp.Uri.to_path (uri t))

let syntax t = Syntax.of_language_id (Lsp.Text_document.languageId t.tdoc)

let timer t = t.timer

let source doc = doc.source

let with_pipeline (doc : t) f =
  Scheduler.async doc.merlin (fun () ->
      Mpipeline.with_pipeline doc.pipeline (fun () -> f doc.pipeline))
  |> Scheduler.await_no_cancel

let with_pipeline_exn doc f =
  let open Fiber.O in
  let+ res = with_pipeline doc f in
  Result.ok_exn res

let version doc = Lsp.Text_document.version doc.tdoc

let make_config uri =
  let path = Lsp.Uri.to_path uri in
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
  let tdoc = Lsp.Text_document.make tdoc in
  (* we can do that b/c all text positions in LSP are line/col *)
  let text = Lsp.Text_document.text tdoc in
  let config = make_config (Lsp.Text_document.documentUri tdoc) in
  let source = Msource.make text in
  let pipeline = Mpipeline.make config source in
  { tdoc; source; config; pipeline; merlin = merlin_thread; timer }

let update_text ?version change doc =
  let tdoc = Lsp.Text_document.apply_content_change ?version change doc.tdoc in
  let text = Lsp.Text_document.text tdoc in
  let config = make_config (Lsp.Text_document.documentUri tdoc) in
  let source = Msource.make text in
  let pipeline = Mpipeline.make config source in
  { doc with tdoc; config; source; pipeline }

let dispatch (doc : t) command =
  with_pipeline doc (fun pipeline -> Query_commands.dispatch pipeline command)

let dispatch_exn (doc : t) command =
  with_pipeline_exn doc (fun pipeline ->
      Query_commands.dispatch pipeline command)
