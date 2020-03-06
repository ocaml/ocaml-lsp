open Import

type t = string [@@deriving_inline yojson]

let _ = fun (_ : t) -> ()

let t_of_yojson = (string_of_yojson : Ppx_yojson_conv_lib.Yojson.Safe.t -> t)

let _ = t_of_yojson

let yojson_of_t = (yojson_of_string : t -> Ppx_yojson_conv_lib.Yojson.Safe.t)

let _ = yojson_of_t

[@@@end]

let equal = String.equal

let hash = String.hash

let to_dyn = String.to_dyn

let to_string uri = uri

let proto = "file://"

let to_path (uri : t) =
  let path =
    match String.drop_prefix ~prefix:proto uri with
    | Some path -> 
        if Sys.win32 && (Str.string_match (Str.regexp "$/[cC]:/") path 0) then
          String.sub path ~pos: 1 ~len: (String.length path - 1)
        else
          path
    | None -> uri
  in
  path
  |> String.replace_all ~pattern:"\\" ~with_:"/"
  |> String.replace_all ~pattern:"%3A" ~with_:":"
  |> String.replace_all ~pattern:"%5C" ~with_:"/"

let of_path (path : string) =
  let path =
    path
    |> String.replace_all ~pattern:"\\" ~with_:"/"
    |> String.replace_all ~pattern:":" ~with_:"%3A"
  in
  proto ^ path

let pp fmt uri = Format.fprintf fmt "%s" uri
