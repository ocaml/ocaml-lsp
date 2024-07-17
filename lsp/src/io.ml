open Import

exception Error of string

let () =
  Printexc.register_printer (function
    | Error msg -> Some ("Error: " ^ msg)
    | _ -> None)
;;

let caseless_equal a b =
  if a == b
  then true
  else (
    let len = String.length a in
    len = String.length b
    &&
    let stop = ref false in
    let idx = ref 0 in
    while (not !stop) && !idx < len do
      let c1 = String.unsafe_get a !idx in
      let c2 = String.unsafe_get b !idx in
      if Char.lowercase_ascii c1 <> Char.lowercase_ascii c2 then stop := true;
      incr idx
    done;
    not !stop)
;;

let content_type_lowercase = String.lowercase_ascii Header.Private.Key.content_type
let content_length_lowercase = String.lowercase_ascii Header.Private.Key.content_length

module Make
    (Io : sig
       type 'a t

       val return : 'a -> 'a t
       val raise : exn -> 'a t

       module O : sig
         val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
         val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
       end
     end)
    (Chan : sig
       type input
       type output

       val read_line : input -> string option Io.t
       val read_exactly : input -> int -> string option Io.t
       val write : output -> string list -> unit Io.t
     end) =
struct
  open Io.O

  let read_header =
    let init_content_length = -1 in
    let rec loop chan content_length content_type =
      let* line = Chan.read_line chan in
      match line with
      | None -> Io.return None
      | Some "" | Some "\r" -> Io.return (Some (content_length, content_type))
      | Some line ->
        (match String.lsplit2 ~on:':' line with
         | None -> loop chan content_length content_type
         | Some (k, v) ->
           let k = String.trim k in
           if caseless_equal k content_length_lowercase
              && content_length = init_content_length
           then (
             let content_length = int_of_string_opt (String.trim v) in
             match content_length with
             | None -> Io.raise (Error "Content-Length is invalid")
             | Some content_length -> loop chan content_length content_type)
           else if caseless_equal k content_type_lowercase && content_type = None
           then (
             let content_type = String.trim v in
             loop chan content_length (Some content_type))
           else loop chan content_length content_type)
    in
    fun chan ->
      let open Io.O in
      let* res = loop chan init_content_length None in
      match res with
      | None -> Io.return None
      | Some (content_length, content_type) ->
        let+ () =
          if content_length = init_content_length
          then Io.raise (Error "content length absent")
          else Io.return ()
        in
        Some (Header.create ?content_type ~content_length ())
  ;;

  let read chan =
    let* header = read_header chan in
    match header with
    | None -> Io.return None
    | Some header ->
      let len = Header.content_length header in
      let* buf = Chan.read_exactly chan len in
      (match buf with
       | None -> Io.raise (Error "unable to read json")
       | Some buf ->
         let json = Json.of_string buf in
         Io.return (Some (Jsonrpc.Packet.t_of_yojson json)))
  ;;

  let write chan packet =
    let json = Jsonrpc.Packet.yojson_of_t packet in
    let data = Json.to_string json in
    let content_length = String.length data in
    let header = Header.create ~content_length () in
    Chan.write chan [ Header.to_string header; data ]
  ;;
end
