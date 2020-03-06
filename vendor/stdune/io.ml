let close_in = close_in

let close_out = close_out

let close_both (ic, oc) =
  match close_out oc with
  | () -> close_in ic
  | exception exn ->
    close_in ic;
    Exn.reraise exn

let input_lines =
  let rec loop ic acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | line -> loop ic (line :: acc)
  in
  fun ic -> loop ic []

let copy_channels =
  let buf_len = 65536 in
  let buf = Bytes.create buf_len in
  let rec loop ic oc =
    match input ic buf 0 buf_len with
    | 0 -> ()
    | n ->
      output oc buf 0 n;
      loop ic oc
  in
  loop

module type S = sig
  type path

  val open_in : ?binary:bool (* default true *) -> path -> in_channel

  val open_out : ?binary:bool (* default true *) -> path -> out_channel

  val with_file_in :
    ?binary:bool (* default true *) -> path -> f:(in_channel -> 'a) -> 'a

  val with_file_out :
    ?binary:bool (* default true *) -> path -> f:(out_channel -> 'a) -> 'a

  val with_lexbuf_from_file : path -> f:(Lexing.lexbuf -> 'a) -> 'a

  val lines_of_file : path -> string list

  val read_file : ?binary:bool -> path -> string

  val write_file : ?binary:bool -> path -> string -> unit

  val compare_files : path -> path -> Ordering.t

  val compare_text_files : path -> path -> Ordering.t

  val write_lines : ?binary:bool -> path -> string list -> unit

  val copy_file : ?chmod:(int -> int) -> src:path -> dst:path -> unit -> unit

  val setup_copy :
       ?chmod:(int -> int)
    -> src:path
    -> dst:path
    -> unit
    -> in_channel * out_channel

  val file_line : path -> int -> string

  val file_lines : path -> start:int -> stop:int -> (string * string) list
end

module Make (Path : sig
  type t

  val to_string : t -> string
end) =
struct
  type path = Path.t

  let open_in ?(binary = true) p =
    let fn = Path.to_string p in
    if binary then
      Stdlib.open_in_bin fn
    else
      Stdlib.open_in fn

  let open_out ?(binary = true) p =
    let fn = Path.to_string p in
    if binary then
      Stdlib.open_out_bin fn
    else
      Stdlib.open_out fn

  let with_file_in ?binary fn ~f =
    Exn.protectx (open_in ?binary fn) ~finally:close_in ~f

  let with_file_out ?binary p ~f =
    Exn.protectx (open_out ?binary p) ~finally:close_out ~f

  let with_lexbuf_from_file fn ~f =
    with_file_in fn ~f:(fun ic ->
        let lb = Lexing.from_channel ic in
        lb.lex_curr_p <-
          { pos_fname = Path.to_string fn
          ; pos_lnum = 1
          ; pos_bol = 0
          ; pos_cnum = 0
          };
        f lb)

  let read_all ic =
    let len = in_channel_length ic in
    really_input_string ic len

  let read_file ?binary fn = with_file_in fn ~f:read_all ?binary

  let lines_of_file fn = with_file_in fn ~f:input_lines ~binary:false

  let write_file ?binary fn data =
    with_file_out ?binary fn ~f:(fun oc -> output_string oc data)

  let write_lines ?binary fn lines =
    with_file_out ?binary fn ~f:(fun oc ->
        List.iter
          ~f:(fun line ->
            output_string oc line;
            output_string oc "\n")
          lines)

  let read_file_and_normalize_eols fn =
    if not Sys.win32 then
      read_file fn
    else
      let src = read_file fn in
      let len = String.length src in
      let dst = Bytes.create len in
      let rec find_next_crnl i =
        match String.index_from src i '\r' with
        | None -> None
        | Some j ->
          if j + 1 < len && src.[j + 1] = '\n' then
            Some j
          else
            find_next_crnl (j + 1)
      in
      let rec loop src_pos dst_pos =
        match find_next_crnl src_pos with
        | None ->
          let len =
            if len > src_pos && src.[len - 1] = '\r' then
              len - 1 - src_pos
            else
              len - src_pos
          in
          Bytes.blit_string ~src ~src_pos ~dst ~dst_pos ~len;
          Bytes.sub_string dst ~pos:0 ~len:(dst_pos + len)
        | Some i ->
          let len = i - src_pos in
          Bytes.blit_string ~src ~src_pos ~dst ~dst_pos ~len;
          let dst_pos = dst_pos + len in
          Bytes.set dst dst_pos '\n';
          loop (i + 2) (dst_pos + 1)
      in
      loop 0 0

  let compare_text_files fn1 fn2 =
    let s1 = read_file_and_normalize_eols fn1 in
    let s2 = read_file_and_normalize_eols fn2 in
    String.compare s1 s2

  let compare_files fn1 fn2 =
    let s1 = read_file fn1 in
    let s2 = read_file fn2 in
    String.compare s1 s2

  let setup_copy ?(chmod = Fn.id) ~src ~dst () =
    let ic = open_in src in
    let oc =
      try
        let perm =
          (Unix.fstat (Unix.descr_of_in_channel ic)).st_perm |> chmod
        in
        Stdlib.open_out_gen
          [ Open_wronly; Open_creat; Open_trunc; Open_binary ]
          perm (Path.to_string dst)
      with exn ->
        close_in ic;
        Exn.reraise exn
    in
    (ic, oc)

  let copy_file ?chmod ~src ~dst () =
    Exn.protectx (setup_copy ?chmod ~src ~dst ()) ~finally:close_both
      ~f:(fun (ic, oc) -> copy_channels ic oc)

  let file_line path n =
    with_file_in ~binary:false path ~f:(fun ic ->
        for _ = 1 to n - 1 do
          ignore (input_line ic)
        done;
        input_line ic)

  let file_lines path ~start ~stop =
    with_file_in ~binary:true path ~f:(fun ic ->
        let rec aux acc lnum =
          if lnum > stop then
            List.rev acc
          else if lnum < start then (
            ignore (input_line ic);
            aux acc (lnum + 1)
          ) else
            let line = input_line ic in
            aux ((string_of_int lnum, line) :: acc) (lnum + 1)
        in
        aux [] 1)
end

include Make (Path)

module String_path = Make (struct
  type t = string

  let to_string x = x
end)
