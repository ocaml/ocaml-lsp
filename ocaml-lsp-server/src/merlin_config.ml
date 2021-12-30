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
      if Hashtbl.mem tbl b' then
        a
      else (
        Hashtbl.add tbl b' ();
        b :: a
      )
    in
    rev (fold_left ~f ~init:[] lst)

  let filter_dup lst = filter_dup' ~equiv:(fun x -> x) lst
end

type directive = Dot_protocol.directive

type config =
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

let empty_config =
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
   designating implementation/interface suffixes. These would be supplied in the
   .merlin file as:

   SUFFIX .sfx .sfxi *)
let parse_suffix str =
  let trimmed = String.trim str in
  let split_on_white = String.extract_blank_separated_words trimmed in
  if List.length split_on_white != 2 then
    []
  else
    let first, second =
      (List.nth split_on_white 0, List.nth split_on_white 1)
    in
    let first = Option.value_exn first in
    let second = Option.value_exn second in
    if String.get first 0 <> '.' || String.get second 0 <> '.' then
      []
    else
      [ (first, second) ]

let prepend_config ~dir:cwd (directives : directive list) config =
  List.fold_left ~init:(config, []) directives ~f:(fun (config, errors) ->
    function
    | `B path -> ({ config with build_path = path :: config.build_path }, errors)
    | `S path ->
      ({ config with source_path = path :: config.source_path }, errors)
    | `CMI path -> ({ config with cmi_path = path :: config.cmi_path }, errors)
    | `CMT path -> ({ config with cmt_path = path :: config.cmt_path }, errors)
    | `EXT exts ->
      ({ config with extensions = exts @ config.extensions }, errors)
    | `SUFFIX suffix ->
      ({ config with suffixes = parse_suffix suffix @ config.suffixes }, errors)
    | `FLG flags ->
      let flags = { Std.workdir = cwd; workval = flags } in
      ({ config with flags = flags :: config.flags }, errors)
    | `STDLIB path -> ({ config with stdlib = Some path }, errors)
    | `READER reader -> ({ config with reader }, errors)
    | `EXCLUDE_QUERY_DIR -> ({ config with exclude_query_dir = true }, errors)
    | `ERROR_MSG str -> (config, str :: errors))

module Process = struct
  type nonrec t =
    { pid : Pid.t
    ; initial_cwd : string
    ; stdin : out_channel
    ; stdout : in_channel
    ; stderr : in_channel
    }

  let start ~dir =
    match Bin.which "dune" with
    | None ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InternalError
           ~message:"dune binary not found" ())
    | Some prog ->
      let prog = Fpath.to_string prog in
      let stdin_r, stdin_w = Unix.pipe () in
      let stdout_r, stdout_w = Unix.pipe () in
      let stderr_r, stderr_w = Unix.pipe () in
      Unix.set_close_on_exec stdin_w;
      let pid =
        let argv = [ prog; "ocaml-merlin"; "--no-print-directory" ] in
        Pid.of_int
          (Spawn.spawn ~cwd:(Path dir) ~prog ~argv ~stdin:stdin_r
             ~stdout:stdout_w ~stderr:stderr_w ())
      in
      Unix.close stdin_r;
      Unix.close stdout_w;
      Unix.close stderr_w;
      let stdin = Unix.out_channel_of_descr stdin_w in
      let stdout = Unix.in_channel_of_descr stdout_r in
      let stderr = Unix.in_channel_of_descr stderr_r in
      let initial_cwd = Misc.canonicalize_filename dir in
      { pid; initial_cwd; stdin; stdout; stderr }
end

let postprocess_config config =
  let clean list = List.rev (List.filter_dup list) in
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

type t =
  { running : (string, Process.t) Table.t
  ; pool : Fiber.Pool.t
  }

let create () =
  { running = Table.create (module String) 0; pool = Fiber.Pool.create () }

let run t = Fiber.Pool.run t.pool

let stop t = Fiber.Pool.stop t.pool

let get_process t ~dir =
  match Table.find t.running dir with
  | Some p -> Fiber.return p
  | None ->
    let p = Process.start ~dir in
    Table.add_exn t.running dir p;
    let+ () =
      Fiber.Pool.task t.pool ~f:(fun () ->
          let+ _status = Lev_fiber.waitpid ~pid:(Pid.to_int p.pid) in
          Table.remove t.running dir)
    in
    p

type context =
  { workdir : string
  ; process_dir : string
  }

let get_config db { workdir; process_dir } path_abs =
  let query path (p : Process.t) =
    Dot_protocol.Commands.send_file ~out_channel:p.stdin path;
    flush p.stdin;
    Dot_protocol.read ~in_channel:p.stdout
  in
  let+ p = get_process db ~dir:process_dir in
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
           else
             path)
  in

  let path =
    match path_rel with
    | Some path_rel -> path_rel
    | _ -> path_abs
  in

  (* Starting with Dune 2.8.3 relative paths are prefered. However to maintain
     compatibility with 2.8 <= Dune <= 2.8.2 we always retry with an absolute
     path if using a relative one failed *)
  let answer =
    match query path p with
    | Ok [ `ERROR_MSG _ ] -> query path_abs p
    | answer -> answer
  in

  match answer with
  | Ok directives ->
    let cfg, failures = prepend_config ~dir:workdir directives empty_config in
    (postprocess_config cfg, failures)
  | Error (Dot_protocol.Unexpected_output msg) -> (empty_config, [ msg ])
  | Error (Dot_protocol.Csexp_parse_error _) ->
    ( empty_config
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
      if List.exists ~f:file_exists fnames then
        Some dir
      else
        None
  in

  let rec loop workdir dir =
    match
      List.find_map [ "dune-project"; "dune-workspace" ] ~f:(fun f ->
          let fname = Filename.concat dir f in
          if file_exists fname then
            let workdir = Option.value ~default:dir workdir in
            Some ({ workdir; process_dir = dir }, fname)
          else
            None)
    with
    | Some s -> Some s
    | None ->
      let parent = Filename.dirname dir in
      if parent <> dir then
        (* Was this directory the workdir ? *)
        let workdir = map_workdir dir workdir in
        loop workdir parent
      else
        None
  in
  loop None start_dir

let get_external_config db (t : Mconfig.t) path =
  let path = Misc.canonicalize_filename path in
  let directory = Filename.dirname path in
  match find_project_context directory with
  | None -> Fiber.return t
  | Some (ctxt, config_path) ->
    let+ dot, failures = get_config db ctxt path in
    let merlin = t.merlin in
    let merlin =
      { merlin with
        build_path = dot.build_path @ merlin.build_path
      ; source_path = dot.source_path @ merlin.source_path
      ; cmi_path = dot.cmi_path @ merlin.cmi_path
      ; cmt_path = dot.cmt_path @ merlin.cmt_path
      ; exclude_query_dir = dot.exclude_query_dir || merlin.exclude_query_dir
      ; extensions = dot.extensions @ merlin.extensions
      ; suffixes = dot.suffixes @ merlin.suffixes
      ; stdlib =
          (if dot.stdlib = None then
            merlin.stdlib
          else
            dot.stdlib)
      ; reader =
          (if dot.reader = [] then
            merlin.reader
          else
            dot.reader)
      ; flags_to_apply = dot.flags @ merlin.flags_to_apply
      ; failures = failures @ merlin.failures
      ; config_path = Some config_path
      }
    in
    Mconfig.normalize { t with merlin }
