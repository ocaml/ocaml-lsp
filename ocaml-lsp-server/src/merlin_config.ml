(* {{{ COPYING *(

   This file is part of Merlin, an helper for ocaml editors

   Copyright (C) 2013 - 2015 Frédéric Bour <frederic.bour(_)lakaban.net> Thomas
   Refis <refis.thomas(_)gmail.com> Simon Castellan <simon.castellan(_)iuwt.fr>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   The Software is provided "as is", without warranty of any kind, express or
   implied, including but not limited to the warranties of merchantability,
   fitness for a particular purpose and noninfringement. In no event shall the
   authors or copyright holders be liable for any claim, damages or other
   liability, whether in an action of contract, tort or otherwise, arising from,
   out of or in connection with the software or the use or other dealings in the
   Software.

   )* }}} *)

open Import
open Fiber.O
module Std = Merlin_utils.Std
module Misc = Merlin_utils.Misc

module List = struct
  include List

  let filter_dup' ~equiv lst =
    let tbl = Hashtbl.create 17 in
    let f a b =
      let b' = equiv b in
      if Hashtbl.mem tbl b' then a
      else (
        Hashtbl.add tbl b' ();
        b :: a)
    in
    rev (fold_left ~f ~init:[] lst)

  let filter_dup lst = filter_dup' ~equiv:(fun x -> x) lst
end

module Config = struct
  type t =
    { build_path : string list
    ; source_path : string list
    ; cmi_path : string list
    ; cmt_path : string list
    ; flags : string list Std.with_workdir list
    ; extensions : string list
    ; suffixes : (string * string) list
    ; stdlib : string option
    ; reader : string list
    ; exclude_query_dir : bool
    }

  let empty =
    { build_path = []
    ; source_path = []
    ; cmi_path = []
    ; cmt_path = []
    ; extensions = []
    ; suffixes = []
    ; flags = []
    ; stdlib = None
    ; reader = []
    ; exclude_query_dir = false
    }

  (* Parses suffixes pairs that were supplied as whitespace separated pairs
     designating implementation/interface suffixes. These would be supplied in
     the .merlin file as:

     SUFFIX .sfx .sfxi *)
  let parse_suffix str =
    match
      let trimmed = String.trim str in
      String.extract_blank_separated_words trimmed
    with
    | [ first; second ] ->
      if String.get first 0 <> '.' || String.get second 0 <> '.' then []
      else [ (first, second) ]
    | _ -> []

  let prepend ~dir:cwd (directives : Merlin_dot_protocol.directive list) config
      =
    List.fold_left ~init:(config, []) directives ~f:(fun (config, errors) ->
      function
      | `B path ->
        ({ config with build_path = path :: config.build_path }, errors)
      | `S path ->
        ({ config with source_path = path :: config.source_path }, errors)
      | `CMI path -> ({ config with cmi_path = path :: config.cmi_path }, errors)
      | `CMT path -> ({ config with cmt_path = path :: config.cmt_path }, errors)
      | `EXT exts ->
        ({ config with extensions = exts @ config.extensions }, errors)
      | `SUFFIX suffix ->
        ( { config with suffixes = parse_suffix suffix @ config.suffixes }
        , errors )
      | `FLG flags ->
        let flags = { Std.workdir = cwd; workval = flags } in
        ({ config with flags = flags :: config.flags }, errors)
      | `STDLIB path -> ({ config with stdlib = Some path }, errors)
      | `READER reader -> ({ config with reader }, errors)
      | `EXCLUDE_QUERY_DIR -> ({ config with exclude_query_dir = true }, errors)
      | `UNKNOWN_TAG _ ->
        (* For easier forward compatibility we ignore unknown configuration tags
           when they are provided by dune *)
        (config, errors)
      | `ERROR_MSG str -> (config, str :: errors))

  let postprocess =
    let clean list = List.rev (List.filter_dup list) in
    fun config ->
      { build_path = clean config.build_path
      ; source_path = clean config.source_path
      ; cmi_path = clean config.cmi_path
      ; cmt_path = clean config.cmt_path
      ; extensions = clean config.extensions
      ; suffixes = clean config.suffixes
      ; flags = clean config.flags
      ; stdlib = config.stdlib
      ; reader = config.reader
      ; exclude_query_dir = config.exclude_query_dir
      }

  let merge t (merlin : Mconfig.merlin) failures config_path =
    { merlin with
      build_path = t.build_path @ merlin.build_path
    ; source_path = t.source_path @ merlin.source_path
    ; cmi_path = t.cmi_path @ merlin.cmi_path
    ; cmt_path = t.cmt_path @ merlin.cmt_path
    ; exclude_query_dir = t.exclude_query_dir || merlin.exclude_query_dir
    ; extensions = t.extensions @ merlin.extensions
    ; suffixes = t.suffixes @ merlin.suffixes
    ; stdlib = (if t.stdlib = None then merlin.stdlib else t.stdlib)
    ; reader = (if t.reader = [] then merlin.reader else t.reader)
    ; flags_to_apply = t.flags @ merlin.flags_to_apply
    ; failures = failures @ merlin.failures
    ; config_path = Some config_path
    }
end

module Process = struct
  type nonrec t =
    { pid : Pid.t
    ; initial_cwd : string
    ; stdin : Lev_fiber.Io.output Lev_fiber.Io.t
    ; stdout : Lev_fiber.Io.input Lev_fiber.Io.t
    ; session : Lev_fiber_csexp.Session.t
    }

  let to_dyn { pid; initial_cwd; _ } =
    let open Dyn in
    record [ ("pid", Pid.to_dyn pid); ("initial_cwd", string initial_cwd) ]

  let waitpid t =
    let+ status = Lev_fiber.waitpid ~pid:(Pid.to_int t.pid) in
    (match status with
    | Unix.WEXITED n -> (
      match n with
      | 0 -> ()
      | n -> Format.eprintf "dune finished with code = %d@.%!" n)
    | WSIGNALED s -> Format.eprintf "dune finished signal = %d@.%!" s
    | WSTOPPED _ -> ());
    Format.eprintf "closed merlin process@.%s@." (Dyn.to_string @@ to_dyn t);
    Lev_fiber.Io.close t.stdin;
    Lev_fiber.Io.close t.stdout

  let start ~dir =
    match Bin.which "dune" with
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make
           ~code:InternalError
           ~message:"dune binary not found"
           ())
    | Some prog ->
      let prog = Fpath.to_string prog in
      let stdin_r, stdin_w = Unix.pipe () in
      let stdout_r, stdout_w = Unix.pipe () in
      Unix.set_close_on_exec stdin_w;
      let pid =
        let argv = [ prog; "ocaml-merlin"; "--no-print-directory" ] in
        Pid.of_int
          (Spawn.spawn
             ~cwd:(Path dir)
             ~prog
             ~argv
             ~stdin:stdin_r
             ~stdout:stdout_w
             ())
      in
      Unix.close stdin_r;
      Unix.close stdout_w;
      let blockity =
        if Sys.win32 then `Blocking
        else (
          Unix.set_nonblock stdin_w;
          Unix.set_nonblock stdout_r;
          `Non_blocking true)
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* stdin = make stdin_w Output in
      let+ stdout = make stdout_r Input in
      let session = Lev_fiber_csexp.Session.create ~socket:false stdout stdin in
      { pid; initial_cwd = dir; stdin; stdout; session }
end

module Dot_protocol_io =
  Merlin_dot_protocol.Make
    (Fiber)
    (struct
      include Lev_fiber_csexp.Session

      let write t x = write t [ x ]
    end)

let should_read_dot_merlin = ref false

type db =
  { running : (string, entry) Table.t
  ; pool : Fiber.Pool.t
  }

and entry =
  { db : db
  ; process : Process.t
  ; mutable ref_count : int
  }

module Entry = struct
  type t = entry

  let create db process = { db; process; ref_count = 0 }

  let equal = ( == )

  let incr t = t.ref_count <- t.ref_count + 1

  let destroy (t : t) =
    assert (t.ref_count > 0);
    t.ref_count <- t.ref_count - 1;
    if t.ref_count > 0 then Fiber.return ()
    else (
      Table.remove t.db.running t.process.initial_cwd;
      Format.eprintf
        "halting dune merlin process@.%s@."
        (Dyn.to_string (Process.to_dyn t.process));
      Dot_protocol_io.Commands.halt t.process.session)
end

let get_process t ~dir =
  match Table.find t.running dir with
  | Some p -> Fiber.return p
  | None ->
    let* process = Process.start ~dir in
    let entry = Entry.create t process in
    Table.add_exn t.running dir entry;
    let+ () = Fiber.Pool.task t.pool ~f:(fun () -> Process.waitpid process) in
    entry

type context =
  { workdir : string
  ; process_dir : string
  }

let get_config (p : Process.t) ~workdir path_abs =
  let query path (p : Process.t) =
    let* () = Dot_protocol_io.Commands.send_file p.session path in
    Dot_protocol_io.read p.session
  in
  (* Both [p.initial_cwd] and [path_abs] have gone through
     [canonicalize_filename] *)
  let path_rel =
    String.drop_prefix ~prefix:p.initial_cwd path_abs
    |> Option.map ~f:(fun path ->
           (* We need to remove the leading path separator after chopping. There
              is one case where no separator is left: when [initial_cwd] was the
              root of the filesystem *)
           if String.length path > 0 && path.[0] = Filename.dir_sep.[0] then
             String.drop path 1
           else path)
  in

  let path =
    match path_rel with
    | Some path_rel -> path_rel
    | _ -> path_abs
  in

  (* Starting with Dune 2.8.3 relative paths are prefered. However to maintain
     compatibility with 2.8 <= Dune <= 2.8.2 we always retry with an absolute
     path if using a relative one failed *)
  let+ answer =
    let* query_path = query path p in
    match query_path with
    | Ok [ `ERROR_MSG _ ] -> query path_abs p
    | answer -> Fiber.return answer
  in

  match answer with
  | Ok directives ->
    let cfg, failures = Config.prepend ~dir:workdir directives Config.empty in
    (Config.postprocess cfg, failures)
  | Error (Merlin_dot_protocol.Unexpected_output msg) -> (Config.empty, [ msg ])
  | Error (Csexp_parse_error _) ->
    ( Config.empty
    , [ "ocamllsp could not load its configuration from the external reader. \
         Building your project with `dune` might solve this issue."
      ] )

let file_exists fname =
  (* XXX stat? *)
  Sys.file_exists fname && not (Sys.is_directory fname)

let find_project_context start_dir =
  (* The workdir is the first directory we find which contains a [dune] file. We
     need to keep track of this folder because [dune ocaml-merlin] might be
     started from a folder that is a parent of the [workdir]. Thus we cannot
     always use that starting folder as the workdir. *)
  let map_workdir dir = function
    | Some dir -> Some dir
    | None ->
      (* XXX what's ["dune-file"]? *)
      let fnames = List.map ~f:(Filename.concat dir) [ "dune"; "dune-file" ] in
      if List.exists ~f:file_exists fnames then Some dir else None
  in

  let rec loop workdir dir =
    match
      List.find_map [ "dune-project"; "dune-workspace" ] ~f:(fun f ->
          let fname = Filename.concat dir f in
          if file_exists fname then
            let workdir =
              Misc.canonicalize_filename (Option.value ~default:dir workdir)
            in
            Some ({ workdir; process_dir = dir }, fname)
          else None)
    with
    | Some s -> Some s
    | None ->
      let parent = Filename.dirname dir in
      if parent <> dir then
        (* Was this directory the workdir ? *)
        let workdir = map_workdir dir workdir in
        loop workdir parent
      else None
  in
  loop None start_dir

type nonrec t =
  { path : string
  ; directory : string
  ; initial : Mconfig.t
  ; mutable entry : Entry.t option
  ; db : db
  }

let destroy t =
  let* () = Fiber.return () in
  match t.entry with
  | None -> Fiber.return ()
  | Some entry ->
    t.entry <- None;
    Entry.destroy entry

let create db path =
  let path =
    let path = Uri.to_path path in
    Misc.canonicalize_filename path
  in
  let directory = Filename.dirname path in
  let initial =
    let filename = Filename.basename path in
    let init = Mconfig.initial in
    { init with
      ocaml = { init.ocaml with real_paths = false }
    ; query = { init.query with filename; directory }
    }
  in
  { path; directory; initial; db; entry = None }

let config (t : t) : Mconfig.t Fiber.t =
  let use_entry entry =
    Entry.incr entry;
    t.entry <- Some entry
  in
  let* () = Fiber.return () in
  match find_project_context t.directory with
  | None ->
    let+ () = destroy t in
    t.initial
  | Some (ctx, config_path) ->
    let* entry = get_process t.db ~dir:ctx.process_dir in
    let* () =
      match t.entry with
      | None ->
        use_entry entry;
        Fiber.return ()
      | Some entry' ->
        if Entry.equal entry entry' then Fiber.return ()
        else
          let+ () = destroy t in
          use_entry entry
    in
    let+ dot, failures = get_config entry.process ~workdir:ctx.workdir t.path in

    if !should_read_dot_merlin && dot = Config.empty then
      Mconfig.get_external_config t.path t.initial
    else
      let merlin = Config.merge dot t.initial.merlin failures config_path in
      Mconfig.normalize { t.initial with merlin }

module DB = struct
  type t = db

  let get t uri = create t uri

  let create () =
    { running = Table.create (module String) 0; pool = Fiber.Pool.create () }

  let run t = Fiber.Pool.run t.pool

  let stop t =
    let* () = Fiber.return () in
    Table.iter t.running ~f:(fun running ->
        let pid = Pid.to_int running.process.pid in
        Unix.kill pid Sys.sigkill);
    Fiber.Pool.stop t.pool
end
