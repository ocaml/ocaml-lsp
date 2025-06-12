open Core
open Async

type t =
  { event_index : int
  ; action : string
  ; file : string list
  ; lines : int list
  ; hash : int list
  ; position : (int * int) list
  }
[@@deriving sexp]

let make
  ~event_index
  ~action
  ~uris
  ~texts
  ~(positions : Position.t list)
  ?(analyze_files = true)
  ()
  : t
  =
  (* We now do the line-counting up-front to avoid duplicating work. *)
  let lines, hash =
    match analyze_files with
    | true ->
      List.map ~f:(String.count ~f:(Char.( = ) '\n')) texts, List.map ~f:Base.String.hash texts
    | false -> [], []
  in
  let position = List.map positions ~f:(fun p -> p.line, p.character) in
  let file = List.map ~f:Lsp.Uri.to_path uris in
  { event_index; action; file; lines; hash; position }
;;

let log_lsp_timing ~wall_time (t : t) =
  if not am_running_test
  then
    [%log.global.info
      "lsp timing"
        (wall_time : float)
        ~event_index:(t.event_index : int)
        ~action:(t.action : string)
        ~file:(t.file : string list)
        ~lines:(t.lines : int list)
        ~hash:(t.hash : int list)
        ~position:(t.position : (int * int) list)]
;;

let with_logging ~f t =
  let start_time = Core_unix.gettimeofday () in
  let res = f () in
  let stop_time = Core_unix.gettimeofday () in
  let wall_time = (stop_time -. start_time) *. 1000. in
  log_lsp_timing ~wall_time t;
  res
;;

let with_fiber_logging ~f t =
  let open Fiber.O in
  let start_time = Core_unix.gettimeofday () in
  let+ res = f () in
  let stop_time = Core_unix.gettimeofday () in
  let wall_time = (stop_time -. start_time) *. 1000. in
  log_lsp_timing ~wall_time t;
  res
;;

let log_merlin_timing ~wall_time ~timing_breakdown t =
  if not am_running_test
  then (
    let total_time =
      List.fold ~f:(fun total ((_ : string), t) -> total +. t) ~init:0. timing_breakdown
    in
    let extract component =
      Option.value
        ~default:Float.nan
        (List.Assoc.find timing_breakdown ~equal:String.equal component)
    in
    let error = extract "error" in
    let pp = extract "pp" in
    let ppx = extract "ppx" in
    let reader = extract "reader" in
    let typer = extract "typer" in
    [%log.global.info
      "merlin timing"
        (wall_time : float)
        (total_time : float)
        (error : float)
        (pp : float)
        (ppx : float)
        (reader : float)
        (typer : float)
        ~event_index:(t.event_index : int)
        ~action:(t.action : string)
        ~file:(t.file : string list)
        ~lines:(t.lines : int list)
        ~hash:(t.hash : int list)
        ~position:(t.position : (int * int) list)])
;;

let log_event_start t =
  if not am_running_test
  then
    [%log.global.info
      "lsp event"
        ~event_index:(t.event_index : int)
        ~action:(t.action : string)
        ~file:(t.file : string list)
        ~lines:(t.lines : int list)
        ~hash:(t.hash : int list)
        ~position:(t.position : (int * int) list)]
;;
